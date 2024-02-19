with System;
with interfaces.C.Strings;

package curl_header is

   package IC renames interfaces.C;

   type CURLX is new System.Address;
   subtype Void_Ptr is System.Address;

   type curl_slist is
      record
         data : IC.Strings.chars_ptr;
         next : System.Address;
      end record;
   pragma Convention (C, curl_slist);

   type access_curl_slist is access curl_slist;

   wrong_curl_type : exception;

   type CURLcode is
     (CURLE_OK,
      CURLE_UNSUPPORTED_PROTOCOL,
      CURLE_FAILED_INIT,
      CURLE_URL_MALFORMAT,
      CURLE_NOT_BUILT_IN,
      CURLE_COULDNT_RESOLVE_PROXY,
      CURLE_COULDNT_RESOLVE_HOST,
      CURLE_COULDNT_CONNECT,
      CURLE_WEIRD_SERVER_REPLY,
      CURLE_REMOTE_ACCESS_DENIED,
      CURLE_FTP_ACCEPT_FAILED,
      CURLE_FTP_WEIRD_PASS_REPLY,
      CURLE_FTP_ACCEPT_TIMEOUT,
      CURLE_FTP_WEIRD_PASV_REPLY,
      CURLE_FTP_WEIRD_227_FORMAT,
      CURLE_FTP_CANT_GET_HOST,
      CURLE_HTTP2,
      CURLE_FTP_COULDNT_SET_TYPE,
      CURLE_PARTIAL_FILE,
      CURLE_FTP_COULDNT_RETR_FILE,
      CURLE_QUOTE_ERROR,
      CURLE_HTTP_RETURNED_ERROR,
      CURLE_WRITE_ERROR,
      CURLE_UPLOAD_FAILED,
      CURLE_READ_ERROR,
      CURLE_OUT_OF_MEMORY,
      CURLE_OPERATION_TIMEDOUT,
      CURLE_FTP_PORT_FAILED,
      CURLE_FTP_COULDNT_USE_REST,
      CURLE_RANGE_ERROR,
      CURLE_HTTP_POST_ERROR,
      CURLE_SSL_CONNECT_ERROR,
      CURLE_BAD_DOWNLOAD_RESUME,
      CURLE_FILE_COULDNT_READ_FILE,
      CURLE_LDAP_CANNOT_BIND,
      CURLE_LDAP_SEARCH_FAILED,
      CURLE_FUNCTION_NOT_FOUND,
      CURLE_ABORTED_BY_CALLBACK,
      CURLE_BAD_FUNCTION_ARGUMENT,
      CURLE_INTERFACE_FAILED,
      CURLE_TOO_MANY_REDIRECTS,
      CURLE_UNKNOWN_OPTION,
      CURLE_SETOPT_OPTION_SYNTAX,
      CURLE_GOT_NOTHING,
      CURLE_SSL_ENGINE_NOTFOUND,
      CURLE_SSL_ENGINE_SETFAILED,
      CURLE_SEND_ERROR,
      CURLE_RECV_ERROR,
      CURLE_SSL_CERTPROBLEM,
      CURLE_SSL_CIPHER,
      CURLE_PEER_FAILED_VERIFICATION,
      CURLE_BAD_CONTENT_ENCODING,
      CURLE_FILESIZE_EXCEEDED,
      CURLE_USE_SSL_FAILED,
      CURLE_SEND_FAIL_REWIND,
      CURLE_SSL_ENGINE_INITFAILED,
      CURLE_LOGIN_DENIED,
      CURLE_TFTP_NOTFOUND,
      CURLE_TFTP_PERM,
      CURLE_REMOTE_DISK_FULL,
      CURLE_TFTP_ILLEGAL,
      CURLE_TFTP_UNKNOWNID,
      CURLE_REMOTE_FILE_EXISTS,
      CURLE_TFTP_NOSUCHUSER,
      CURLE_SSL_CACERT_BADFILE,
      CURLE_REMOTE_FILE_NOT_FOUND,
      CURLE_SSH,
      CURLE_SSL_SHUTDOWN_FAILED,
      CURLE_AGAIN,
      CURLE_SSL_CRL_BADFILE,
      CURLE_SSL_ISSUER_ERROR,
      CURLE_FTP_PRET_FAILED,
      CURLE_RTSP_CSEQ_ERROR,
      CURLE_RTSP_SESSION_ERROR,
      CURLE_FTP_BAD_FILE_LIST,
      CURLE_CHUNK_FAILED,
      CURLE_NO_CONNECTION_AVAILABLE,
      CURLE_SSL_PINNEDPUBKEYNOTMATCH,
      CURLE_SSL_INVALIDCERTSTATUS,
      CURLE_HTTP2_STREAM,
      CURLE_RECURSIVE_API_CALL,
      CURLE_AUTH_ERROR,
      CURLE_HTTP3,
      CURLE_QUIC_CONNECT_ERROR,
      CURLE_PROXY,
      CURLE_SSL_CLIENTCERT,
      CURLE_UNRECOVERABLE_POLL,
      CURLE_TOO_LARGE,
      CURL_LAST
     );

   for CURLcode use
     (CURLE_OK                       => 0,
      CURLE_UNSUPPORTED_PROTOCOL     => 1,
      CURLE_FAILED_INIT              => 2,
      CURLE_URL_MALFORMAT            => 3,
      CURLE_NOT_BUILT_IN             => 4,
      CURLE_COULDNT_RESOLVE_PROXY    => 5,
      CURLE_COULDNT_RESOLVE_HOST     => 6,
      CURLE_COULDNT_CONNECT          => 7,
      CURLE_WEIRD_SERVER_REPLY       => 8,
      CURLE_REMOTE_ACCESS_DENIED     => 9,
      CURLE_FTP_ACCEPT_FAILED        => 10,
      CURLE_FTP_WEIRD_PASS_REPLY     => 11,
      CURLE_FTP_ACCEPT_TIMEOUT       => 12,
      CURLE_FTP_WEIRD_PASV_REPLY     => 13,
      CURLE_FTP_WEIRD_227_FORMAT     => 14,
      CURLE_FTP_CANT_GET_HOST        => 15,
      CURLE_HTTP2                    => 16,
      CURLE_FTP_COULDNT_SET_TYPE     => 17,
      CURLE_PARTIAL_FILE             => 18,
      CURLE_FTP_COULDNT_RETR_FILE    => 19,
      CURLE_QUOTE_ERROR              => 21,
      CURLE_HTTP_RETURNED_ERROR      => 22,
      CURLE_WRITE_ERROR              => 23,
      CURLE_UPLOAD_FAILED            => 25,
      CURLE_READ_ERROR               => 26,
      CURLE_OUT_OF_MEMORY            => 27,
      CURLE_OPERATION_TIMEDOUT       => 28,
      CURLE_FTP_PORT_FAILED          => 30,
      CURLE_FTP_COULDNT_USE_REST     => 31,
      CURLE_RANGE_ERROR              => 33,
      CURLE_HTTP_POST_ERROR          => 34,
      CURLE_SSL_CONNECT_ERROR        => 35,
      CURLE_BAD_DOWNLOAD_RESUME      => 36,
      CURLE_FILE_COULDNT_READ_FILE   => 37,
      CURLE_LDAP_CANNOT_BIND         => 38,
      CURLE_LDAP_SEARCH_FAILED       => 39,
      CURLE_FUNCTION_NOT_FOUND       => 41,
      CURLE_ABORTED_BY_CALLBACK      => 42,
      CURLE_BAD_FUNCTION_ARGUMENT    => 43,
      CURLE_INTERFACE_FAILED         => 45,
      CURLE_TOO_MANY_REDIRECTS       => 47,
      CURLE_UNKNOWN_OPTION           => 48,
      CURLE_SETOPT_OPTION_SYNTAX     => 49,
      CURLE_GOT_NOTHING              => 52,
      CURLE_SSL_ENGINE_NOTFOUND      => 53,
      CURLE_SSL_ENGINE_SETFAILED     => 54,
      CURLE_SEND_ERROR               => 55,
      CURLE_RECV_ERROR               => 56,
      CURLE_SSL_CERTPROBLEM          => 58,
      CURLE_SSL_CIPHER               => 59,
      CURLE_PEER_FAILED_VERIFICATION => 60,
      CURLE_BAD_CONTENT_ENCODING     => 61,
      CURLE_FILESIZE_EXCEEDED        => 63,
      CURLE_USE_SSL_FAILED           => 64,
      CURLE_SEND_FAIL_REWIND         => 65,
      CURLE_SSL_ENGINE_INITFAILED    => 66,
      CURLE_LOGIN_DENIED             => 67,
      CURLE_TFTP_NOTFOUND            => 68,
      CURLE_TFTP_PERM                => 69,
      CURLE_REMOTE_DISK_FULL         => 70,
      CURLE_TFTP_ILLEGAL             => 71,
      CURLE_TFTP_UNKNOWNID           => 72,
      CURLE_REMOTE_FILE_EXISTS       => 73,
      CURLE_TFTP_NOSUCHUSER          => 74,
      CURLE_SSL_CACERT_BADFILE       => 77,
      CURLE_REMOTE_FILE_NOT_FOUND    => 78,
      CURLE_SSH                      => 79,
      CURLE_SSL_SHUTDOWN_FAILED      => 80,
      CURLE_AGAIN                    => 81,
      CURLE_SSL_CRL_BADFILE          => 82,
      CURLE_SSL_ISSUER_ERROR         => 83,
      CURLE_FTP_PRET_FAILED          => 84,
      CURLE_RTSP_CSEQ_ERROR          => 85,
      CURLE_RTSP_SESSION_ERROR       => 86,
      CURLE_FTP_BAD_FILE_LIST        => 87,
      CURLE_CHUNK_FAILED             => 88,
      CURLE_NO_CONNECTION_AVAILABLE  => 89,
      CURLE_SSL_PINNEDPUBKEYNOTMATCH => 90,
      CURLE_SSL_INVALIDCERTSTATUS    => 91,
      CURLE_HTTP2_STREAM             => 92,
      CURLE_RECURSIVE_API_CALL       => 93,
      CURLE_AUTH_ERROR               => 94,
      CURLE_HTTP3                    => 95,
      CURLE_QUIC_CONNECT_ERROR       => 96,
      CURLE_PROXY                    => 97,
      CURLE_SSL_CLIENTCERT           => 98,
      CURLE_UNRECOVERABLE_POLL       => 99,
      CURLE_TOO_LARGE                => 100,
      CURL_LAST                      => 999
     );
   pragma Convention (C, CURLcode);

   type OptionString is
     (CURLOPT_URL,
      CURLOPT_PROXY,
      CURLOPT_USERPWD,
      CURLOPT_PROXYUSERPWD,
      CURLOPT_RANGE,
      CURLOPT_ERRORBUFFER,
      CURLOPT_REFERER,
      CURLOPT_FTPPORT,
      CURLOPT_USERAGENT,
      CURLOPT_COOKIE,
      CURLOPT_SSLCERT,
      CURLOPT_KEYPASSWD,
      CURLOPT_COOKIEFILE,
      CURLOPT_CUSTOMREQUEST,
      CURLOPT_INTERFACE,
      CURLOPT_KRBLEVEL,
      CURLOPT_CAINFO,
      CURLOPT_RANDOM_FILE,
      CURLOPT_EGDSOCKET,
      CURLOPT_COOKIEJAR,
      CURLOPT_SSL_CIPHER_LIST,
      CURLOPT_SSLCERTTYPE,
      CURLOPT_SSLKEY,
      CURLOPT_SSLKEYTYPE,
      CURLOPT_SSLENGINE,
      CURLOPT_CAPATH,
      CURLOPT_ACCEPT_ENCODING,
      CURLOPT_NETRC_FILE,
      CURLOPT_FTP_ACCOUNT,
      CURLOPT_COOKIELIST,
      CURLOPT_FTP_ALTERNATIVE_TO_USER,
      CURLOPT_SSH_PUBLIC_KEYFILE,
      CURLOPT_SSH_PRIVATE_KEYFILE,
      CURLOPT_SSH_HOST_PUBLIC_KEY_MD5,
      CURLOPT_CRLFILE,
      CURLOPT_ISSUERCERT,
      CURLOPT_USERNAME,
      CURLOPT_PASSWORD,
      CURLOPT_PROXYUSERNAME,
      CURLOPT_PROXYPASSWORD,
      CURLOPT_NOPROXY,
      CURLOPT_SSH_KNOWNHOSTS,
      CURLOPT_MAIL_FROM,
      CURLOPT_RTSP_SESSION_ID,
      CURLOPT_RTSP_STREAM_URI,
      CURLOPT_RTSP_TRANSPORT,
      CURLOPT_TLSAUTH_USERNAME,
      CURLOPT_TLSAUTH_PASSWORD,
      CURLOPT_TLSAUTH_TYPE,
      CURLOPT_DNS_SERVERS,
      CURLOPT_XOAUTH2_BEARER,
      CURLOPT_DNS_INTERFACE,
      CURLOPT_DNS_LOCAL_IP4,
      CURLOPT_DNS_LOCAL_IP6,
      CURLOPT_LOGIN_OPTIONS,
      CURLOPT_PINNEDPUBLICKEY,
      CURLOPT_UNIX_SOCKET_PATH,
      CURLOPT_PROXY_SERVICE_NAME,
      CURLOPT_SERVICE_NAME,
      CURLOPT_DEFAULT_PROTOCOL,
      CURLOPT_PROXY_CAINFO,
      CURLOPT_PROXY_CAPATH,
      CURLOPT_PROXY_TLSAUTH_USERNAME,
      CURLOPT_PROXY_TLSAUTH_PASSWORD,
      CURLOPT_PROXY_TLSAUTH_TYPE,
      CURLOPT_PROXY_SSLCERT,
      CURLOPT_PROXY_SSLCERTTYPE,
      CURLOPT_PROXY_SSLKEY,
      CURLOPT_PROXY_SSLKEYTYPE,
      CURLOPT_PROXY_KEYPASSWD,
      CURLOPT_PROXY_SSL_CIPHER_LIST,
      CURLOPT_PROXY_CRLFILE,
      CURLOPT_PRE_PROXY,
      CURLOPT_PROXY_PINNEDPUBLICKEY,
      CURLOPT_ABSTRACT_UNIX_SOCKET,
      CURLOPT_REQUEST_TARGET,
      CURLOPT_TLS13_CIPHERS,
      CURLOPT_PROXY_TLS13_CIPHERS,
      CURLOPT_DOH_URL,
      CURLOPT_ALTSVC,
      CURLOPT_SASL_AUTHZID,
      CURLOPT_PROXY_ISSUERCERT,
      CURLOPT_SSH_HOST_PUBLIC_KEY_SHA256
     );

   for OptionString use
     (CURLOPT_URL                     => 10002,
      CURLOPT_PROXY                   => 10004,
      CURLOPT_USERPWD                 => 10005,
      CURLOPT_PROXYUSERPWD            => 10006,
      CURLOPT_RANGE                   => 10007,
      CURLOPT_ERRORBUFFER             => 10010,
      CURLOPT_REFERER                 => 10016,
      CURLOPT_FTPPORT                 => 10017,
      CURLOPT_USERAGENT               => 10018,
      CURLOPT_COOKIE                  => 10022,
      CURLOPT_SSLCERT                 => 10025,
      CURLOPT_KEYPASSWD               => 10026,
      CURLOPT_COOKIEFILE              => 10031,
      CURLOPT_CUSTOMREQUEST           => 10036,
      CURLOPT_INTERFACE               => 10062,
      CURLOPT_KRBLEVEL                => 10063,
      CURLOPT_CAINFO                  => 10065,
      CURLOPT_RANDOM_FILE             => 10076,
      CURLOPT_EGDSOCKET               => 10077,
      CURLOPT_COOKIEJAR               => 10082,
      CURLOPT_SSL_CIPHER_LIST         => 10083,
      CURLOPT_SSLCERTTYPE             => 10086,
      CURLOPT_SSLKEY                  => 10087,
      CURLOPT_SSLKEYTYPE              => 10088,
      CURLOPT_SSLENGINE               => 10089,
      CURLOPT_CAPATH                  => 10097,
      CURLOPT_ACCEPT_ENCODING         => 10102,
      CURLOPT_NETRC_FILE              => 10118,
      CURLOPT_FTP_ACCOUNT             => 10134,
      CURLOPT_COOKIELIST              => 10135,
      CURLOPT_FTP_ALTERNATIVE_TO_USER => 10147,
      CURLOPT_SSH_PUBLIC_KEYFILE      => 10152,
      CURLOPT_SSH_PRIVATE_KEYFILE     => 10153,
      CURLOPT_SSH_HOST_PUBLIC_KEY_MD5 => 10162,
      CURLOPT_CRLFILE                 => 10169,
      CURLOPT_ISSUERCERT              => 10170,
      CURLOPT_USERNAME                => 10173,
      CURLOPT_PASSWORD                => 10174,
      CURLOPT_PROXYUSERNAME           => 10175,
      CURLOPT_PROXYPASSWORD           => 10176,
      CURLOPT_NOPROXY                 => 10177,
      CURLOPT_SSH_KNOWNHOSTS          => 10183,
      CURLOPT_MAIL_FROM               => 10186,
      CURLOPT_RTSP_SESSION_ID         => 10190,
      CURLOPT_RTSP_STREAM_URI         => 10191,
      CURLOPT_RTSP_TRANSPORT          => 10192,
      CURLOPT_TLSAUTH_USERNAME        => 10204,
      CURLOPT_TLSAUTH_PASSWORD        => 10205,
      CURLOPT_TLSAUTH_TYPE            => 10206,
      CURLOPT_DNS_SERVERS             => 10211,
      CURLOPT_XOAUTH2_BEARER          => 10220,
      CURLOPT_DNS_INTERFACE           => 10221,
      CURLOPT_DNS_LOCAL_IP4           => 10222,
      CURLOPT_DNS_LOCAL_IP6           => 10223,
      CURLOPT_LOGIN_OPTIONS           => 10224,
      CURLOPT_PINNEDPUBLICKEY         => 10230,
      CURLOPT_UNIX_SOCKET_PATH        => 10231,
      CURLOPT_PROXY_SERVICE_NAME      => 10235,
      CURLOPT_SERVICE_NAME            => 10236,
      CURLOPT_DEFAULT_PROTOCOL        => 10238,
      CURLOPT_PROXY_CAINFO            => 10246,
      CURLOPT_PROXY_CAPATH            => 10247,
      CURLOPT_PROXY_TLSAUTH_USERNAME  => 10251,
      CURLOPT_PROXY_TLSAUTH_PASSWORD  => 10252,
      CURLOPT_PROXY_TLSAUTH_TYPE      => 10253,
      CURLOPT_PROXY_SSLCERT           => 10254,
      CURLOPT_PROXY_SSLCERTTYPE       => 10255,
      CURLOPT_PROXY_SSLKEY            => 10256,
      CURLOPT_PROXY_SSLKEYTYPE        => 10257,
      CURLOPT_PROXY_KEYPASSWD         => 10258,
      CURLOPT_PROXY_SSL_CIPHER_LIST   => 10259,
      CURLOPT_PROXY_CRLFILE           => 10260,
      CURLOPT_PRE_PROXY               => 10262,
      CURLOPT_PROXY_PINNEDPUBLICKEY   => 10263,
      CURLOPT_ABSTRACT_UNIX_SOCKET    => 10264,
      CURLOPT_REQUEST_TARGET          => 10266,
      CURLOPT_TLS13_CIPHERS           => 10276,
      CURLOPT_PROXY_TLS13_CIPHERS     => 10277,
      CURLOPT_DOH_URL                 => 10279,
      CURLOPT_ALTSVC                  => 10287,
      CURLOPT_SASL_AUTHZID            => 10289,
      CURLOPT_PROXY_ISSUERCERT        => 10296,
      CURLOPT_SSH_HOST_PUBLIC_KEY_SHA256 => 10311
     );
   pragma Convention (C, OptionString);

   type OptionLong is
     (CURLOPT_PORT,
      CURLOPT_TIMEOUT,
      CURLOPT_INFILESIZE,
      CURLOPT_LOW_SPEED_LIMIT,
      CURLOPT_LOW_SPEED_TIME,
      CURLOPT_RESUME_FROM,
      CURLOPT_SSLVERSION,
      CURLOPT_TIMECONDITION,
      CURLOPT_TIMEVALUE,
      CURLOPT_NETRC,
      CURLOPT_PROXYPORT,
      CURLOPT_POSTFIELDSIZE,
      CURLOPT_HTTPPROXYTUNNEL,
      CURLOPT_MAXREDIRS,
      CURLOPT_MAXCONNECTS,
      CURLOPT_CONNECTTIMEOUT,
      CURLOPT_SSL_VERIFYHOST,
      CURLOPT_HTTP_VERSION,
      CURLOPT_SSLENGINE_DEFAULT,
      CURLOPT_DNS_CACHE_TIMEOUT,
      CURLOPT_COOKIESESSION,
      CURLOPT_BUFFERSIZE,
      CURLOPT_PROXYTYPE,
      CURLOPT_HTTPAUTH,
      CURLOPT_PROXYAUTH,
      CURLOPT_SERVER_RESPONSE_TIMEOUT,
      CURLOPT_IPRESOLVE,
      CURLOPT_MAXFILESIZE,
      CURLOPT_USE_SSL,
      CURLOPT_FTPSSLAUTH,
      CURLOPT_LOCALPORT,
      CURLOPT_LOCALPORTRANGE,
      CURLOPT_SSH_AUTH_TYPES,
      CURLOPT_FTP_SSL_CCC,
      CURLOPT_TIMEOUT_MS,
      CURLOPT_CONNECTTIMEOUT_MS,
      CURLOPT_NEW_FILE_PERMS,
      CURLOPT_NEW_DIRECTORY_PERMS,
      CURLOPT_POSTREDIR,
      CURLOPT_ADDRESS_SCOPE,
      CURLOPT_TFTP_BLKSIZE,
      CURLOPT_SOCKS5_GSSAPI_NEC,
      CURLOPT_RTSP_CLIENT_CSEQ,
      CURLOPT_RTSP_SERVER_CSEQ,
      CURLOPT_ACCEPTTIMEOUT_MS,
      CURLOPT_TCP_KEEPIDLE,
      CURLOPT_TCP_KEEPINTVL,
      CURLOPT_SSL_OPTIONS,
      CURLOPT_SASL_IR,
      CURLOPT_EXPECT_100_TIMEOUT_MS,
      CURLOPT_HEADEROPT,
      CURLOPT_STREAM_WEIGHT,
      CURLOPT_PROXY_SSL_VERIFYHOST,
      CURLOPT_PROXY_SSLVERSION,
      CURLOPT_PROXY_SSL_OPTIONS,
      CURLOPT_SUPPRESS_CONNECT_HEADERS,
      CURLOPT_SOCKS5_AUTH,
      CURLOPT_HAPPY_EYEBALLS_TIMEOUT_MS,
      CURLOPT_DISALLOW_USERNAME_IN_URL,
      CURLOPT_UPLOAD_BUFFERSIZE,
      CURLOPT_UPKEEP_INTERVAL_MS,
      CURLOPT_ALTSVC_CTRL,
      CURLOPT_MAXAGE_CONN,
      CURLOPT_MAIL_RCPT_ALLOWFAILS,
      CURLOPT_HSTS_CTRL,
      CURLOPT_DOH_SSL_VERIFYHOST,
      CURLOPT_MAXLIFETIME_CONN,
      CURLOPT_MIME_OPTIONS,
      CURLOPT_WS_OPTIONS,
      CURLOPT_CA_CACHE_TIMEOUT,
      CURLOPT_SERVER_RESPONSE_TIMEOUT_MS
     );
   for OptionLong use
     (CURLOPT_PORT                       => 3,
      CURLOPT_TIMEOUT                    => 13,
      CURLOPT_INFILESIZE                 => 14,
      CURLOPT_LOW_SPEED_LIMIT            => 19,
      CURLOPT_LOW_SPEED_TIME             => 20,
      CURLOPT_RESUME_FROM                => 21,
      CURLOPT_SSLVERSION                 => 32,
      CURLOPT_TIMECONDITION              => 33,
      CURLOPT_TIMEVALUE                  => 34,
      CURLOPT_NETRC                      => 51,
      CURLOPT_PROXYPORT                  => 59,
      CURLOPT_POSTFIELDSIZE              => 60,
      CURLOPT_HTTPPROXYTUNNEL            => 61,
      CURLOPT_MAXREDIRS                  => 68,
      CURLOPT_MAXCONNECTS                => 71,
      CURLOPT_CONNECTTIMEOUT             => 78,
      CURLOPT_SSL_VERIFYHOST             => 81,
      CURLOPT_HTTP_VERSION               => 84,
      CURLOPT_SSLENGINE_DEFAULT          => 90,
      CURLOPT_DNS_CACHE_TIMEOUT          => 92,
      CURLOPT_COOKIESESSION              => 96,
      CURLOPT_BUFFERSIZE                 => 98,
      CURLOPT_PROXYTYPE                  => 101,
      CURLOPT_HTTPAUTH                   => 107,
      CURLOPT_PROXYAUTH                  => 111,
      CURLOPT_SERVER_RESPONSE_TIMEOUT    => 112,
      CURLOPT_IPRESOLVE                  => 113,
      CURLOPT_MAXFILESIZE                => 114,
      CURLOPT_USE_SSL                    => 119,
      CURLOPT_FTPSSLAUTH                 => 129,
      CURLOPT_LOCALPORT                  => 139,
      CURLOPT_LOCALPORTRANGE             => 140,
      CURLOPT_SSH_AUTH_TYPES             => 151,
      CURLOPT_FTP_SSL_CCC                => 154,
      CURLOPT_TIMEOUT_MS                 => 155,
      CURLOPT_CONNECTTIMEOUT_MS          => 156,
      CURLOPT_NEW_FILE_PERMS             => 159,
      CURLOPT_NEW_DIRECTORY_PERMS        => 160,
      CURLOPT_POSTREDIR                  => 161,
      CURLOPT_ADDRESS_SCOPE              => 171,
      CURLOPT_TFTP_BLKSIZE               => 178,
      CURLOPT_SOCKS5_GSSAPI_NEC          => 180,
      CURLOPT_RTSP_CLIENT_CSEQ           => 193,
      CURLOPT_RTSP_SERVER_CSEQ           => 194,
      CURLOPT_ACCEPTTIMEOUT_MS           => 212,
      CURLOPT_TCP_KEEPIDLE               => 214,
      CURLOPT_TCP_KEEPINTVL              => 215,
      CURLOPT_SSL_OPTIONS                => 216,
      CURLOPT_SASL_IR                    => 218,
      CURLOPT_EXPECT_100_TIMEOUT_MS      => 227,
      CURLOPT_HEADEROPT                  => 229,
      CURLOPT_STREAM_WEIGHT              => 239,
      CURLOPT_PROXY_SSL_VERIFYHOST       => 249,
      CURLOPT_PROXY_SSLVERSION           => 250,
      CURLOPT_PROXY_SSL_OPTIONS          => 261,
      CURLOPT_SUPPRESS_CONNECT_HEADERS   => 265,
      CURLOPT_SOCKS5_AUTH                => 267,
      CURLOPT_HAPPY_EYEBALLS_TIMEOUT_MS  => 271,
      CURLOPT_DISALLOW_USERNAME_IN_URL   => 278,
      CURLOPT_UPLOAD_BUFFERSIZE          => 280,
      CURLOPT_UPKEEP_INTERVAL_MS         => 281,
      CURLOPT_ALTSVC_CTRL                => 286,
      CURLOPT_MAXAGE_CONN                => 288,
      CURLOPT_MAIL_RCPT_ALLOWFAILS       => 290,
      CURLOPT_HSTS_CTRL                  => 299,
      CURLOPT_DOH_SSL_VERIFYHOST         => 307,
      CURLOPT_MAXLIFETIME_CONN           => 314,
      CURLOPT_MIME_OPTIONS               => 315,
      CURLOPT_WS_OPTIONS                 => 320,
      CURLOPT_CA_CACHE_TIMEOUT           => 321,
      CURLOPT_SERVER_RESPONSE_TIMEOUT_MS => 324
     );
   pragma Convention (C, OptionLong);

   type OptionBool is
     (CURLOPT_CRLF,
      CURLOPT_VERBOSE,
      CURLOPT_HEADER,
      CURLOPT_NOPROGRESS,
      CURLOPT_NOBODY,
      CURLOPT_FAILONERROR,
      CURLOPT_UPLOAD,
      CURLOPT_POST,
      CURLOPT_DIRLISTONLY,
      CURLOPT_APPEND,
      CURLOPT_FOLLOWLOCATION,
      CURLOPT_TRANSFERTEXT,
      CURLOPT_AUTOREFERER,
      CURLOPT_SSL_VERIFYPEER,
      CURLOPT_FILETIME,
      CURLOPT_FRESH_CONNECT,
      CURLOPT_FORBID_REUSE,
      CURLOPT_HTTPGET,
      CURLOPT_FTP_USE_EPSV,
      CURLOPT_NOSIGNAL,
      CURLOPT_UNRESTRICTED_AUTH,
      CURLOPT_FTP_USE_EPRT,
      CURLOPT_FTP_CREATE_MISSING_DIRS,
      CURLOPT_TCP_NODELAY,
      CURLOPT_IGNORE_CONTENT_LENGTH,
      CURLOPT_FTP_SKIP_PASV_IP,
      CURLOPT_CONNECT_ONLY,
      CURLOPT_SSL_SESSIONID_CACHE,
      CURLOPT_HTTP_TRANSFER_DECODING,
      CURLOPT_HTTP_CONTENT_DECODING,
      CURLOPT_PROXY_TRANSFER_MODE,
      CURLOPT_CERTINFO,
      CURLOPT_FTP_USE_PRET,
      CURLOPT_WILDCARDMATCH,
      CURLOPT_TRANSFER_ENCODING,
      CURLOPT_TCP_KEEPALIVE,
      CURLOPT_SSL_ENABLE_ALPN,
      CURLOPT_SSL_VERIFYSTATUS,
      CURLOPT_SSL_FALSESTART,
      CURLOPT_PATH_AS_IS,
      CURLOPT_PIPEWAIT,
      CURLOPT_TFTP_NO_OPTIONS,
      CURLOPT_TCP_FASTOPEN,
      CURLOPT_KEEP_SENDING_ON_ERROR,
      CURLOPT_PROXY_SSL_VERIFYPEER,
      CURLOPT_SSH_COMPRESSION,
      CURLOPT_HAPROXYPROTOCOL,
      CURLOPT_DNS_SHUFFLE_ADDRESSES,
      CURLOPT_HTTP09_ALLOWED,
      CURLOPT_DOH_SSL_VERIFYPEER,
      CURLOPT_DOH_SSL_VERIFYSTATUS,
      CURLOPT_QUICK_EXIT
     );
   for OptionBool use
     (CURLOPT_CRLF                       => 27,
      CURLOPT_VERBOSE                    => 41,
      CURLOPT_HEADER                     => 42,
      CURLOPT_NOPROGRESS                 => 43,
      CURLOPT_NOBODY                     => 44,
      CURLOPT_FAILONERROR                => 45,
      CURLOPT_UPLOAD                     => 46,
      CURLOPT_POST                       => 47,
      CURLOPT_DIRLISTONLY                => 48,
      CURLOPT_APPEND                     => 50,
      CURLOPT_FOLLOWLOCATION             => 52,
      CURLOPT_TRANSFERTEXT               => 53,
      CURLOPT_AUTOREFERER                => 58,
      CURLOPT_SSL_VERIFYPEER             => 64,
      CURLOPT_FILETIME                   => 69,
      CURLOPT_FRESH_CONNECT              => 74,
      CURLOPT_FORBID_REUSE               => 75,
      CURLOPT_HTTPGET                    => 80,
      CURLOPT_FTP_USE_EPSV               => 85,
      CURLOPT_NOSIGNAL                   => 99,
      CURLOPT_UNRESTRICTED_AUTH          => 105,
      CURLOPT_FTP_USE_EPRT               => 106,
      CURLOPT_FTP_CREATE_MISSING_DIRS    => 110,
      CURLOPT_TCP_NODELAY                => 121,
      CURLOPT_IGNORE_CONTENT_LENGTH      => 136,
      CURLOPT_FTP_SKIP_PASV_IP           => 137,
      CURLOPT_CONNECT_ONLY               => 141,
      CURLOPT_SSL_SESSIONID_CACHE        => 150,
      CURLOPT_HTTP_TRANSFER_DECODING     => 157,
      CURLOPT_HTTP_CONTENT_DECODING      => 158,
      CURLOPT_PROXY_TRANSFER_MODE        => 166,
      CURLOPT_CERTINFO                   => 172,
      CURLOPT_FTP_USE_PRET               => 188,
      CURLOPT_WILDCARDMATCH              => 197,
      CURLOPT_TRANSFER_ENCODING          => 207,
      CURLOPT_TCP_KEEPALIVE              => 213,
      CURLOPT_SSL_ENABLE_ALPN            => 226,
      CURLOPT_SSL_VERIFYSTATUS           => 232,
      CURLOPT_SSL_FALSESTART             => 233,
      CURLOPT_PATH_AS_IS                 => 234,
      CURLOPT_PIPEWAIT                   => 237,
      CURLOPT_TFTP_NO_OPTIONS            => 242,
      CURLOPT_TCP_FASTOPEN               => 244,
      CURLOPT_KEEP_SENDING_ON_ERROR      => 245,
      CURLOPT_PROXY_SSL_VERIFYPEER       => 248,
      CURLOPT_SSH_COMPRESSION            => 268,
      CURLOPT_HAPROXYPROTOCOL            => 274,
      CURLOPT_DNS_SHUFFLE_ADDRESSES      => 275,
      CURLOPT_HTTP09_ALLOWED             => 285,
      CURLOPT_DOH_SSL_VERIFYPEER         => 306,
      CURLOPT_DOH_SSL_VERIFYSTATUS       => 308,
      CURLOPT_QUICK_EXIT                 => 322
    );
   pragma Convention (C, OptionBool);

   type OptionCallback is
     (CURLOPT_WRITEFUNCTION,
      CURLOPT_READFUNCTION,
      CURLOPT_HEADERFUNCTION,
      CURLOPT_DEBUGFUNCTION,
      CURLOPT_SSL_CTX_FUNCTION,
      CURLOPT_IOCTLFUNCTION,
      CURLOPT_SOCKOPTFUNCTION,
      CURLOPT_OPENSOCKETFUNCTION,
      CURLOPT_SEEKFUNCTION,
      CURLOPT_SSH_KEYFUNCTION,
      CURLOPT_INTERLEAVEFUNCTION,
      CURLOPT_CHUNK_BGN_FUNCTION,
      CURLOPT_CHUNK_END_FUNCTION,
      CURLOPT_FNMATCH_FUNCTION,
      CURLOPT_CLOSESOCKETFUNCTION,
      CURLOPT_XFERINFOFUNCTION,
      CURLOPT_RESOLVER_START_FUNCTION,
      CURLOPT_TRAILERFUNCTION,
      CURLOPT_HSTSREADFUNCTION,
      CURLOPT_HSTSWRITEFUNCTION,
      CURLOPT_PREREQFUNCTION,
      CURLOPT_SSH_HOSTKEYFUNCTION
     );
   for OptionCallback use
     (CURLOPT_WRITEFUNCTION              => 20011,
      CURLOPT_READFUNCTION               => 20012,
      CURLOPT_HEADERFUNCTION             => 20079,
      CURLOPT_DEBUGFUNCTION              => 20094,
      CURLOPT_SSL_CTX_FUNCTION           => 20108,
      CURLOPT_IOCTLFUNCTION              => 20130,
      CURLOPT_SOCKOPTFUNCTION            => 20148,
      CURLOPT_OPENSOCKETFUNCTION         => 20163,
      CURLOPT_SEEKFUNCTION               => 20167,
      CURLOPT_SSH_KEYFUNCTION            => 20184,
      CURLOPT_INTERLEAVEFUNCTION         => 20196,
      CURLOPT_CHUNK_BGN_FUNCTION         => 20198,
      CURLOPT_CHUNK_END_FUNCTION         => 20199,
      CURLOPT_FNMATCH_FUNCTION           => 20200,
      CURLOPT_CLOSESOCKETFUNCTION        => 20208,
      CURLOPT_XFERINFOFUNCTION           => 20219,
      CURLOPT_RESOLVER_START_FUNCTION    => 20272,
      CURLOPT_TRAILERFUNCTION            => 20283,
      CURLOPT_HSTSREADFUNCTION           => 20301,
      CURLOPT_HSTSWRITEFUNCTION          => 20303,
      CURLOPT_PREREQFUNCTION             => 20312,
      CURLOPT_SSH_HOSTKEYFUNCTION        => 20316
      );
   pragma Convention (C, OptionCallback);

   type OptionPointer is
     (CURLOPT_WRITEDATA,
      CURLOPT_READDATA,
      CURLOPT_ERRORBUFFER,
      CURLOPT_POSTFIELDS,
      CURLOPT_HTTPHEADER,
      CURLOPT_QUOTE,
      CURLOPT_HEADERDATA,
      CURLOPT_STDERR,
      CURLOPT_POSTQUOTE,
      CURLOPT_XFERINFODATA,
      CURLOPT_TELNETOPTIONS,
      CURLOPT_PREQUOTE,
      CURLOPT_DEBUGDATA,
      CURLOPT_SHARE,
      CURLOPT_PRIVATE,
      CURLOPT_HTTP200ALIASES,
      CURLOPT_SSL_CTX_DATA,
      CURLOPT_IOCTLDATA,
      CURLOPT_SOCKOPTDATA,
      CURLOPT_OPENSOCKETDATA,
      CURLOPT_COPYPOSTFIELDS,
      CURLOPT_SEEKDATA,
      CURLOPT_SSH_KEYDATA,
      CURLOPT_MAIL_RCPT,
      CURLOPT_INTERLEAVEDATA,
      CURLOPT_CHUNK_DATA,
      CURLOPT_FNMATCH_DATA,
      CURLOPT_RESOLVE,
      CURLOPT_CLOSESOCKETDATA,
      CURLOPT_PROXYHEADER,
      CURLOPT_STREAM_DEPENDS,
      CURLOPT_STREAM_DEPENDS_E,
      CURLOPT_CONNECT_TO,
      CURLOPT_MIMEPOST,
      CURLOPT_RESOLVER_START_DATA,
      CURLOPT_CURLU,
      CURLOPT_TRAILERDATA,
      CURLOPT_HSTSREADDATA,
      CURLOPT_HSTSWRITEDATA,
      CURLOPT_PREREQDATA,
      CURLOPT_SSH_HOSTKEYDATA
     );

   for OptionPointer use
     (CURLOPT_WRITEDATA           => 10001,
      CURLOPT_READDATA            => 10009,
      CURLOPT_ERRORBUFFER         => 10010,
      CURLOPT_POSTFIELDS          => 10015,
      CURLOPT_HTTPHEADER          => 10023,
      CURLOPT_QUOTE               => 10028,
      CURLOPT_HEADERDATA          => 10029,
      CURLOPT_STDERR              => 10037,
      CURLOPT_POSTQUOTE           => 10039,
      CURLOPT_XFERINFODATA        => 10057,
      CURLOPT_TELNETOPTIONS       => 10070,
      CURLOPT_PREQUOTE            => 10093,
      CURLOPT_DEBUGDATA           => 10095,
      CURLOPT_SHARE               => 10100,
      CURLOPT_PRIVATE             => 10103,
      CURLOPT_HTTP200ALIASES      => 10104,
      CURLOPT_SSL_CTX_DATA        => 10109,
      CURLOPT_IOCTLDATA           => 10130,
      CURLOPT_SOCKOPTDATA         => 10149,
      CURLOPT_OPENSOCKETDATA      => 10164,
      CURLOPT_COPYPOSTFIELDS      => 10165,
      CURLOPT_SEEKDATA            => 10168,
      CURLOPT_SSH_KEYDATA         => 10185,
      CURLOPT_MAIL_RCPT           => 10187,
      CURLOPT_INTERLEAVEDATA      => 10195,
      CURLOPT_CHUNK_DATA          => 10201,
      CURLOPT_FNMATCH_DATA        => 10202,
      CURLOPT_RESOLVE             => 10203,
      CURLOPT_CLOSESOCKETDATA     => 10209,
      CURLOPT_PROXYHEADER         => 10228,
      CURLOPT_STREAM_DEPENDS      => 10240,
      CURLOPT_STREAM_DEPENDS_E    => 10241,
      CURLOPT_CONNECT_TO          => 10243,
      CURLOPT_MIMEPOST            => 10269,
      CURLOPT_RESOLVER_START_DATA => 10273,
      CURLOPT_CURLU               => 10282,
      CURLOPT_TRAILERDATA         => 10284,
      CURLOPT_HSTSREADDATA        => 10302,
      CURLOPT_HSTSWRITEDATA       => 10304,
      CURLOPT_PREREQDATA          => 10313,
      CURLOPT_SSH_HOSTKEYDATA     => 10317
     );
   pragma Convention (C, OptionPointer);

   type curl_infotype is
     (CURLINFO_TEXT,
      CURLINFO_HEADER_IN,
      CURLINFO_HEADER_OUT,
      CURLINFO_DATA_IN,
      CURLINFO_DATA_OUT,
      CURLINFO_SSL_DATA_IN,
      CURLINFO_SSL_DATA_OUT,
      CURLINFO_END);
   pragma Convention (C, curl_infotype);

   CURLINFO_STRING : constant Natural := 16#100000#;
   CURLINFO_LONG   : constant Natural := 16#200000#;
   CURLINFO_DOUBLE : constant Natural := 16#300000#;
   CURLINFO_SLIST  : constant Natural := 16#400000#;
   CURLINFO_PTR    : constant Natural := 16#400000#;  --  same as SLIST
   CURLINFO_SOCKET : constant Natural := 16#500000#;
   CURLINFO_OFF_T  : constant Natural := 16#600000#;

   type curl_info is
     (CURLINFO_NONE,
      CURLINFO_LASTONE,
      CURLINFO_EFFECTIVE_URL,
      CURLINFO_CONTENT_TYPE,
      CURLINFO_PRIVATE,
      CURLINFO_FTP_ENTRY_PATH,
      CURLINFO_REDIRECT_URL,
      CURLINFO_PRIMARY_IP,
      CURLINFO_RTSP_SESSION_ID,
      CURLINFO_LOCAL_IP,
      CURLINFO_SCHEME,
      CURLINFO_EFFECTIVE_METHOD,
      CURLINFO_REFERER,
      CURLINFO_CAINFO,
      CURLINFO_CAPATH,
      CURLINFO_RESPONSE_CODE,
      CURLINFO_HEADER_SIZE,
      CURLINFO_REQUEST_SIZE,
      CURLINFO_SSL_VERIFYRESULT,
      CURLINFO_FILETIME,
      CURLINFO_REDIRECT_COUNT,
      CURLINFO_HTTP_CONNECTCODE,
      CURLINFO_HTTPAUTH_AVAIL,
      CURLINFO_PROXYAUTH_AVAIL,
      CURLINFO_OS_ERRNO,
      CURLINFO_NUM_CONNECTS,
      CURLINFO_CONDITION_UNMET,
      CURLINFO_RTSP_CLIENT_CSEQ,
      CURLINFO_RTSP_SERVER_CSEQ,
      CURLINFO_RTSP_CSEQ_RECV,
      CURLINFO_PRIMARY_PORT,
      CURLINFO_LOCAL_PORT,
      CURLINFO_TOTAL_TIME,
      CURLINFO_NAMELOOKUP_TIME,
      CURLINFO_CONNECT_TIME,
      CURLINFO_PRETRANSFER_TIME,
      CURLINFO_STARTTRANSFER_TIME,
      CURLINFO_REDIRECT_TIME,
      CURLINFO_APPCONNECT_TIME,
      CURLINFO_SSL_ENGINES,
      CURLINFO_COOKIELIST,
      CURLINFO_CERTINFO,
      CURLINFO_TLS_SSL_PTR,
      CURLINFO_ACTIVESOCKET,
      CURLINFO_SIZE_UPLOAD_T,
      CURLINFO_SIZE_DOWNLOAD_T,
      CURLINFO_SPEED_DOWNLOAD_T,
      CURLINFO_SPEED_UPLOAD_T,
      CURLINFO_FILETIME_T,
      CURLINFO_CONTENT_LENGTH_DOWNLOAD_T,
      CURLINFO_CONTENT_LENGTH_UPLOAD_T,
      CURLINFO_TOTAL_TIME_T,
      CURLINFO_NAMELOOKUP_TIME_T,
      CURLINFO_CONNECT_TIME_T,
      CURLINFO_PRETRANSFER_TIME_T,
      CURLINFO_STARTTRANSFER_TIME_T,
      CURLINFO_REDIRECT_TIME_T,
      CURLINFO_APPCONNECT_TIME_T,
      CURLINFO_RETRY_AFTER,
      CURLINFO_XFER_ID,
      CURLINFO_CONN_ID,
      CURLINFO_QUEUE_TIME_T
     );

   for curl_info use
     (CURLINFO_NONE                 => 0,
      CURLINFO_LASTONE              => 65,
      CURLINFO_EFFECTIVE_URL        => CURLINFO_STRING + 1,
      CURLINFO_CONTENT_TYPE         => CURLINFO_STRING + 18,
      CURLINFO_PRIVATE              => CURLINFO_STRING + 21,
      CURLINFO_FTP_ENTRY_PATH       => CURLINFO_STRING + 30,
      CURLINFO_REDIRECT_URL         => CURLINFO_STRING + 31,
      CURLINFO_PRIMARY_IP           => CURLINFO_STRING + 32,
      CURLINFO_RTSP_SESSION_ID      => CURLINFO_STRING + 36,
      CURLINFO_LOCAL_IP             => CURLINFO_STRING + 41,
      CURLINFO_SCHEME               => CURLINFO_STRING + 49,
      CURLINFO_EFFECTIVE_METHOD     => CURLINFO_STRING + 58,
      CURLINFO_REFERER              => CURLINFO_STRING + 60,
      CURLINFO_CAINFO               => CURLINFO_STRING + 61,
      CURLINFO_CAPATH               => CURLINFO_STRING + 62,
      CURLINFO_RESPONSE_CODE        => CURLINFO_LONG   + 2,
      CURLINFO_HEADER_SIZE          => CURLINFO_LONG   + 11,
      CURLINFO_REQUEST_SIZE         => CURLINFO_LONG   + 12,
      CURLINFO_SSL_VERIFYRESULT     => CURLINFO_LONG   + 13,
      CURLINFO_FILETIME             => CURLINFO_LONG   + 14,
      CURLINFO_REDIRECT_COUNT       => CURLINFO_LONG   + 20,
      CURLINFO_HTTP_CONNECTCODE     => CURLINFO_LONG   + 22,
      CURLINFO_HTTPAUTH_AVAIL       => CURLINFO_LONG   + 23,
      CURLINFO_PROXYAUTH_AVAIL      => CURLINFO_LONG   + 24,
      CURLINFO_OS_ERRNO             => CURLINFO_LONG   + 25,
      CURLINFO_NUM_CONNECTS         => CURLINFO_LONG   + 26,
      CURLINFO_CONDITION_UNMET      => CURLINFO_LONG   + 35,
      CURLINFO_RTSP_CLIENT_CSEQ     => CURLINFO_LONG   + 37,
      CURLINFO_RTSP_SERVER_CSEQ     => CURLINFO_LONG   + 38,
      CURLINFO_RTSP_CSEQ_RECV       => CURLINFO_LONG   + 39,
      CURLINFO_PRIMARY_PORT         => CURLINFO_LONG   + 40,
      CURLINFO_LOCAL_PORT           => CURLINFO_LONG   + 42,
      CURLINFO_TOTAL_TIME           => CURLINFO_DOUBLE + 3,
      CURLINFO_NAMELOOKUP_TIME      => CURLINFO_DOUBLE + 4,
      CURLINFO_CONNECT_TIME         => CURLINFO_DOUBLE + 5,
      CURLINFO_PRETRANSFER_TIME     => CURLINFO_DOUBLE + 6,
      CURLINFO_STARTTRANSFER_TIME   => CURLINFO_DOUBLE + 17,
      CURLINFO_REDIRECT_TIME        => CURLINFO_DOUBLE + 19,
      CURLINFO_APPCONNECT_TIME      => CURLINFO_DOUBLE + 33,
      CURLINFO_SSL_ENGINES          => CURLINFO_SLIST  + 27,
      CURLINFO_COOKIELIST           => CURLINFO_SLIST  + 28,
      CURLINFO_CERTINFO             => CURLINFO_PTR    + 34,
      CURLINFO_TLS_SSL_PTR          => CURLINFO_PTR    + 45,
      CURLINFO_ACTIVESOCKET         => CURLINFO_SOCKET + 44,
      CURLINFO_SIZE_UPLOAD_T        => CURLINFO_OFF_T  + 7,
      CURLINFO_SIZE_DOWNLOAD_T      => CURLINFO_OFF_T  + 8,
      CURLINFO_SPEED_DOWNLOAD_T     => CURLINFO_OFF_T  + 9,
      CURLINFO_SPEED_UPLOAD_T       => CURLINFO_OFF_T  + 10,
      CURLINFO_FILETIME_T           => CURLINFO_OFF_T  + 14,
      CURLINFO_CONTENT_LENGTH_DOWNLOAD_T => CURLINFO_OFF_T  + 15,
      CURLINFO_CONTENT_LENGTH_UPLOAD_T   => CURLINFO_OFF_T  + 16,
      CURLINFO_TOTAL_TIME_T         => CURLINFO_OFF_T + 50,
      CURLINFO_NAMELOOKUP_TIME_T    => CURLINFO_OFF_T + 51,
      CURLINFO_CONNECT_TIME_T       => CURLINFO_OFF_T + 52,
      CURLINFO_PRETRANSFER_TIME_T   => CURLINFO_OFF_T + 53,
      CURLINFO_STARTTRANSFER_TIME_T => CURLINFO_OFF_T + 54,
      CURLINFO_REDIRECT_TIME_T      => CURLINFO_OFF_T + 55,
      CURLINFO_APPCONNECT_TIME_T    => CURLINFO_OFF_T + 56,
      CURLINFO_RETRY_AFTER          => CURLINFO_OFF_T + 57,
      CURLINFO_XFER_ID              => CURLINFO_OFF_T + 63,
      CURLINFO_CONN_ID              => CURLINFO_OFF_T + 64,
      CURLINFO_QUEUE_TIME_T         => CURLINFO_OFF_T + 65
     );
   pragma Convention (C, curl_info);

   type write_callback is access function
     (ptr      : IC.Strings.chars_ptr;
      size     : IC.size_t;
      nmemb    : IC.size_t;
      userdata : System.Address) return IC.size_t;
   pragma Convention (C, write_callback);

   type debug_callback is access function
     (handle   : CURLX;
      dtype    : curl_infotype;
      data     : IC.Strings.chars_ptr;
      size     : IC.size_t;
      clientp  : Void_Ptr) return IC.int;
   pragma Convention (C, debug_callback);

   type curl_off_t is range -(2**63) .. +(2**63 - 1);

   type progress_callback is access function
     (clientp  : Void_Ptr;
      dltotal  : curl_off_t;
      dlnow    : curl_off_t;
      ultotal  : curl_off_t;
      ulnow    : curl_off_t) return IC.int;
   pragma Convention (C, progress_callback);

   --  initialize curl object
   function curl_easy_init return CURLX;
   pragma Import (C, curl_easy_init, "curl_easy_init");

   --  finalize curl object
   procedure curl_easy_cleanup (curl : CURLX);
   pragma Import (C, curl_easy_cleanup, "curl_easy_cleanup");

   --  free a previously built curl_slist.
   procedure curl_slist_free_all (list : access_curl_slist);
   pragma Import (C, curl_slist_free_all, "curl_slist_free_all");

   --  overloaded procedure to set curl object string properties
   procedure set_curl_option (curlobj : CURLX; option : OptionString; optvalue : String);

   --  overloaded procedure to set curl object integer properties
   procedure set_curl_option (curlobj : CURLX; option : OptionLong; optvalue : Long_Integer);

   --  overloaded procedure to set curl object boolean properties
   procedure set_curl_option (curlobj : CURLX; option : OptionBool; optvalue : Boolean);

   --  overloaded procedure to set curl object pointer properties
   procedure set_curl_option (curlobj : CURLX; option : OptionPointer; optvalue : Void_Ptr);

   --  overloaded procedure to set curl linked string list properties
   procedure set_curl_option (curlobj : CURLX;
                              option  : OptionPointer;
                              optvalue : access_curl_slist);

   --  implement CURLOPT_WRITEFUNCTION, CURLOPT_READFUNCTION, CURLOPT_HEADERFUNCTION
   procedure set_write_callback  (curlobj : CURLX; callback : write_callback);
   procedure set_read_callback   (curlobj : CURLX; callback : write_callback);
   procedure set_header_callback (curlobj : CURLX; callback : write_callback);

   --  CURLOPT_DEBUGFUNCTION
   procedure set_debug_callback (curlobj : CURLX; callback : debug_callback);

   --  CURLOPT_XFERINFOFUNCTION
   procedure set_progress_callback (curlobj : CURLX; callback : progress_callback);

   --  Get long integer curl information
   function get_info_value_long (curlobj : CURLX; info : curl_info) return Long_Integer;

   procedure build_header (list : in out access_curl_slist; header_line : String);

   procedure execute_curl (curlobj : CURLX);

private

   function curl_setopt_string
     (curl   : CURLX;
      option : OptionString;
      value  : IC.Strings.chars_ptr) return CURLcode;
   pragma Import (C, curl_setopt_string, "curl_easy_setopt");

   function curl_setopt_long
     (curl   : CURLX;
      option : OptionLong;
      value  : IC.long) return CURLcode;
   pragma Import (C, curl_setopt_long, "curl_easy_setopt");

   function curl_setopt_bool
     (curl   : CURLX;
      option : OptionBool;
      value  : IC.long) return CURLcode;
   pragma Import (C, curl_setopt_bool, "curl_easy_setopt");

   function curl_setopt_write_callback
     (curl   : CURLX;
      option : OptionCallback;
      value  : write_callback) return CURLcode;
   pragma Import (C, curl_setopt_write_callback, "curl_easy_setopt");

   function curl_setopt_debug_callback
     (curl   : CURLX;
      option : OptionCallback;
      value  : debug_callback) return CURLcode;
   pragma Import (C, curl_setopt_debug_callback, "curl_easy_setopt");

   function curl_setopt_progress_callback
     (curl   : CURLX;
      option : OptionCallback;
      value  : progress_callback) return CURLcode;
   pragma Import (C, curl_setopt_progress_callback, "curl_easy_setopt");

   function curl_setopt_pointer
     (curl   : CURLX;
      option : OptionPointer;
      value  : Void_Ptr) return CURLcode;
   pragma Import (C, curl_setopt_pointer, "curl_easy_setopt");

   function curl_setopt_slist
     (curl   : CURLX;
      option : OptionPointer;
      value  : access_curl_slist) return CURLcode;
   pragma Import (C, curl_setopt_slist, "curl_easy_setopt");

   function curl_easy_perform
     (curl   : CURLX) return CURLcode;
   pragma Import (C, curl_easy_perform, "curl_easy_perform");

   function curl_easy_getinfo_sysaddress
     (curl   : CURLX;
      info   : curl_info;
      value  : Void_Ptr) return CURLcode;
   pragma Import (C, curl_easy_getinfo_sysaddress, "curl_easy_getinfo");

   --  Appends a string to a linked list. If no list exists, it will be created
   --  first. Returns the new list, after appending.
   function curl_slist_append
     (list : access_curl_slist;
      data : IC.Strings.chars_ptr) return access_curl_slist;
   pragma Import (C, curl_slist_append, "curl_slist_append");

end curl_header;
