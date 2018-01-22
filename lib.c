#include <stdio.h>
#include <string.h>
#include <openssl/bio.h>
#include <openssl/err.h>
#include <openssl/pem.h>
#include <openssl/ssl.h>
#include "uv.h"

typedef struct {
  SSL *ssl;
  BIO *reader;
  BIO *writer;
} ssl_client;

ssl_client* new_ssl_client(SSL_CTX* ctx)
{
  ssl_client *sc = malloc(sizeof(ssl_client));
  memset(sc, 0, sizeof(ssl_client));

  sc->reader = BIO_new(BIO_s_mem());
  sc->writer = BIO_new(BIO_s_mem());
  sc->ssl = SSL_new(ctx);

  SSL_set_accept_state(sc->ssl);
  SSL_set_bio(sc->ssl, sc->reader, sc->writer);

  return sc;
}

static int clamp(int x)
{
  return x < 0 ? 0 : x; 
}

int fill_input_buffer(ssl_client *c, char *b, int len)
{
  int n;
  int bytes_read = 0;

  do {
    n = BIO_write(c->reader, b + bytes_read, clamp(len - bytes_read));
    if(n > 0) {
      bytes_read += n;
    }
  } while( n > 0);

  return bytes_read;
}

int drain_output_buffer(ssl_client *c, char *b, int len)
{
  int n;
  int bytes_written = 0;

  do {
    n = BIO_read(c->writer, b + bytes_written, clamp(len - bytes_written));
    if(n > 0) {
      bytes_written += n;
    } else if (!BIO_should_retry(c->writer)) {
      return -1;
    }
  } while( n > 0 );

  return bytes_written;
}

#define DRAIN_OUTPUT_BUFFER -2
#define HANDSHAKE_OK 0

int do_handshake(ssl_client *c)
{
  if(SSL_is_init_finished(c->ssl)) {
    return HANDSHAKE_OK;
  }

  int n = SSL_accept(c->ssl);

  int ssl_err = SSL_get_error(c->ssl, n);

  switch(ssl_err) {
  case SSL_ERROR_WANT_READ:
    return DRAIN_OUTPUT_BUFFER;
  case SSL_ERROR_NONE:
    break;
  case SSL_ERROR_ZERO_RETURN:
  case SSL_ERROR_SYSCALL:
  default:
    return -1;
  }
  return 0;
}


int ssl_read(ssl_client *c, char *b, int len)
{
  int bytes_read = 0;
  int n;
  do {
    n = SSL_read(c->ssl, b + bytes_read, clamp(len - bytes_read));
    if( n > 0 ) {
      bytes_read += n;
    }
  } while( n > 0);

  if( n < 0 ) {
    int ssl_err = SSL_get_error(c->ssl, n);
    switch(ssl_err) {
    case SSL_ERROR_NONE:
      break;
    case SSL_ERROR_WANT_READ:
      return bytes_read == 0 ? DRAIN_OUTPUT_BUFFER : bytes_read;
    case SSL_ERROR_WANT_WRITE:
      return DRAIN_OUTPUT_BUFFER;
    }
  }
  return bytes_read;
}

int ssl_write(ssl_client *c, char *b, int len)
{
  int bytes_written = 0;
  int n;

  do {
    n = SSL_write(c->ssl, b + bytes_written, clamp(len - bytes_written));
    if (n > 0 ) {
      bytes_written += n;
    }
  } while( n > 0);
  if( n < 0 ) {
    int ssl_err = SSL_get_error(c->ssl, n);
    switch(ssl_err) {
    case SSL_ERROR_NONE:
      break;
    case SSL_ERROR_WANT_READ:
      return bytes_written == 0 ? DRAIN_OUTPUT_BUFFER : bytes_written;
    case SSL_ERROR_WANT_WRITE:
      return DRAIN_OUTPUT_BUFFER;
    }
  }

  return bytes_written;
}


void free_ssl_client(ssl_client *c)
{
  SSL_free(c->ssl);
  free(c);
}

SSL_CTX* new_ssl_context(const char* cert, const char* key)
{
  SSL_CTX *ctx = SSL_CTX_new(SSLv23_server_method());
  if (!ctx) {
    return NULL;
  }

  int err = SSL_CTX_use_certificate_file(ctx, cert, SSL_FILETYPE_PEM);
  if (err != 1) {
    goto CLEAN_UP;
  }
  err = SSL_CTX_use_PrivateKey_file(ctx, key, SSL_FILETYPE_PEM);
  if( err != 1 ) {
    goto CLEAN_UP;
  }

  if( SSL_CTX_check_private_key(ctx) != 1) {
    goto CLEAN_UP;
  }

  SSL_CTX_set_options(ctx, SSL_OP_ALL|SSL_OP_NO_SSLv2|SSL_OP_NO_SSLv3);
  return ctx;

 CLEAN_UP:
  SSL_CTX_free(ctx);
  return NULL;
}

void free_ssl_context(SSL_CTX* ssl)
{
  SSL_CTX_free(ssl);
}

void init_ssl(void)
{
  SSL_library_init();
  OpenSSL_add_all_algorithms();
  SSL_load_error_strings();
  ERR_load_BIO_strings();
  ERR_load_crypto_strings();
}

