/*
 *  SPDX-License-Identifier: ISC
 *  Reference: /License.txt
 */

#ifndef _WIN32

#include <string.h>
#include <mbedtls/build_info.h>
#include <mbedtls/platform.h>
#include <mbedtls/error.h>
#include <mbedtls/entropy.h>
#include <mbedtls/ctr_drbg.h>
#include <mbedtls/pk.h>

/*
 * sign 256-bit hash
 * params:
 *    hash (unsigned char pointer to 32 byte buffer)
 *    private key file path (unsigned char pointer)
 *    in/out pointer to 32-byte buffer
 *    in/out output length
 * return int:
 *    0 = success
 *    1 = failure - failed to seed random number generator
 *    2 = failure - failed to parse the private key file
 *    3 = failure - failed to generate the signature of the hash
 */

int
sign_digest (const unsigned char *hash, const size_t hash_len, const char *key_path,
             unsigned char *signature, const size_t sig_capacity, size_t sig_len)
{
  int ret = 0;
  int exit_code = 0;
  mbedtls_pk_context pk;
  mbedtls_entropy_context entropy;
  mbedtls_ctr_drbg_context ctr_drbg;
  const char *pers = "mbedtls_pk_sign";

  /* initialize */
  sig_len = 0;
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

  if ((ret = mbedtls_pk_parse_keyfile (&pk, key_path, NULL, mbedtls_ctr_drbg_random, &ctr_drbg))
      != 0)
    {
      /* failed to parse the private key file */
      exit_code = 2;
      goto exit;
    }

  /* The passed hash will be Blake3 which is a 256-bit hash, so sha256 should cover that case */
  if ((ret = mbedtls_pk_sign (&pk, MBEDTLS_MD_SHA256, hash, hash_len, signature, sig_capacity,
                              &sig_len, mbedtls_ctr_drbg_random, &ctr_drbg))
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
   mbedtls_exit(exit_code);
}

#endif
