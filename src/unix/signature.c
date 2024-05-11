/*
 *  SPDX-License-Identifier: ISC
 *  Reference: /License.txt
 */

#ifndef _WIN32

#include <stdio.h>
#include <string.h>
#include <mbedtls/build_info.h>
#include <mbedtls/platform.h>
#include <mbedtls/error.h>
#include <mbedtls/entropy.h>
#include <mbedtls/ctr_drbg.h>
#include <mbedtls/pk.h>

/*
 * sign hash
 * params:
 *    hash (unsigned char pointer to buffer)
 *    hash_len (length of hash buffer)
 *    private key file path (unsigned char pointer)
 *    signature (unsigned char pointer to buffer)
 *    signature buffer size
 *    final signature size pointer
 * return int:
 *    0 = success
 *    1 = failure - failed to seed random number generator
 *    2 = failure - failed to parse the private key file
 *    3 = failure - failed to generate the signature of the hash
 */

int
sign_digest (const unsigned char *hash, const size_t hash_len, const char *key_path,
             unsigned char *signature, const size_t sig_capacity, size_t *sig_len)
{
  int ret = 0;
  int exit_code = 0;
  mbedtls_pk_context pk;
  mbedtls_entropy_context entropy;
  mbedtls_ctr_drbg_context ctr_drbg;
  const char *pers = "mbedtls_pk_sign";

  /* initialize */
  mbedtls_entropy_init (&entropy);
  mbedtls_ctr_drbg_init (&ctr_drbg);
  mbedtls_pk_init (&pk);

  /* Seed the random number generator */
  if ((ret = mbedtls_ctr_drbg_seed (&ctr_drbg, mbedtls_entropy_func, &entropy,
                                    (const unsigned char *)pers, strlen (pers)))
      != 0)
    {
      /* failed to seed the random number generator */
      exit_code = 1;
      goto exit;
    }

  /* Read private key for use */
  if ((ret = mbedtls_pk_parse_keyfile (&pk, key_path, NULL, mbedtls_ctr_drbg_random, &ctr_drbg))
      != 0)
    {
      /* failed to parse the private key file */
      exit_code = 2;
      goto exit;
    }

  /* The passed hash will be Blake3 which is a 256-bit hash, so sha256 should cover that case */
  if ((ret = mbedtls_pk_sign (&pk, MBEDTLS_MD_SHA256, hash, hash_len, signature, sig_capacity,
                              sig_len, mbedtls_ctr_drbg_random, &ctr_drbg))
      != 0)
    {
      /* failed to generate the signature */
      exit_code = 3;
      goto exit;
    }

  exit_code = 0;

exit:
   mbedtls_pk_free(&pk);
   mbedtls_ctr_drbg_free (&ctr_drbg);
   mbedtls_entropy_free (&entropy);
   return exit_code;
}

/*
 * verify hash
 * params:
 *    hash (unsigned char pointer to buffer)
 *    hash_len (length of hash buffer)
 *    public key file path (unsigned char pointer)
 *    signature file path (unsigned char pointer)
 * return int:
 *    0 = success
 *    1 = failure - failed to parse the public key file
 *    2 = failure - failed to read in signature file data
 *    3 = failure - failed to verify signature against the given hash
 */

int
verify_digest (const unsigned char *hash, const size_t hash_len, const char *public_key_path,
               const char *signature_path)
{
   FILE *f;
   int ret = 0;
   int exit_code = 0;
   size_t sig_len;
   unsigned char signature[1024]; /* limit used for digest */
   mbedtls_pk_context pk;

   /* initialize */
   mbedtls_pk_init (&pk);

   /* Read public key for use */
   if ((ret = mbedtls_pk_parse_public_keyfile (&pk, public_key_path)) != 0)
    {
      /* failed to parse the public key file */
      exit_code = 1;
      goto exit;
    }

   /* read signature file data */
   if ((f = fopen (signature_path, "rb")) == NULL)
    {
      exit_code = 2;
      goto exit;
    }
    sig_len = fread(signature, 1, sizeof(signature), f);
    fclose(f);

    /* verify signature against the given hash */
    if ((ret = mbedtls_pk_verify(&pk, MBEDTLS_MD_SHA256, hash, hash_len,
                                 signature, sig_len)) != 0)
    {
      exit_code = 3;
      goto exit;
    }

exit:
   mbedtls_pk_free (&pk);
   return exit_code;
}

#endif
