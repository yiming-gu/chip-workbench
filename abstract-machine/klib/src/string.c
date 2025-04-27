#include <klib.h>
#include <klib-macros.h>
#include <stdint.h>

#if !defined(__ISA_NATIVE__) || defined(__NATIVE_USE_KLIB__)

size_t strlen(const char *s) {
  size_t i=0;
  for (i=0; s[i]!='\0'; i++);
  return i;
}

char *strcpy(char *dst, const char *src) {
  size_t i;
  for (i=0; src[i]!='\0'; i++) {
    dst[i] = src[i];
  }
  dst[i] = '\0';
  return dst;
}

char *strncpy(char *dst, const char *src, size_t n) {
  size_t i;
  for (i = 0; i < n && src[i] != '\0'; i++)
    dst[i] = src[i];
  for ( ; i < n; i++)
    dst[i] = '\0';
  return dst;
}

char *strcat(char *dst, const char *src) {
  size_t i, dst_len;
  dst_len = strlen(dst);
  for (i=0; src[i]!='\0'; i++) {
    dst[dst_len+i] = src[i];
  }
  dst[dst_len+i] = '\0';
  return dst;
}

int strcmp(const char *s1, const char *s2) {
  size_t i;
  for (i=0; s1[i]!='\0'&&s1[i]==s2[i]; i++) ;
  return s1[i] - s2[i];
}

int strncmp(const char *s1, const char *s2, size_t n) {
  size_t i;
  for (i=0; s1[i]!='\0'&&s1[i]==s2[i]&&i<n-1; i++) ;
  return s1[i] - s2[i];
}

void *memset(void *s, int c, size_t n) {
  size_t i;
  for (i=0; i<n; i++) {
    ((char *)s)[i] = c;
  }
  return s;
}

void *memmove(void *dst, const void *src, size_t n) {
  int32_t i;
  if (dst < src) {
    for (i=0; i<n; i++) {
      ((char *)dst)[i] = ((char *)src)[i];
    }
    return dst;
  }
  else if (dst > src) {
    for (i=n-1; i>=0; i--) {
      ((char *)dst)[i] = ((char *)src)[i];
    }
    return dst;
  }
  else {
    return dst;
  }
}

void *memcpy(void *out, const void *in, size_t n) {
  size_t i;
  for (i=0; i<n; i++) {
    ((char *)out)[i] = ((char *)in)[i];
  }
  return out;
}

int memcmp(const void *s1, const void *s2, size_t n) {
  size_t i;
  for (i=0; i<n-1&&((unsigned char *)s1)[i]==((unsigned char *)s2)[i]; i++) ;
  return ((unsigned char *)s1)[i] - ((unsigned char *)s2)[i];
}

char *itoa(int value, char *str, int radix) {
  char reverse[36];
  char *p = reverse;
  bool sign = (value >= 0) ? true : false;

  value = (value >= 0) ? value : -value;
  *p++ = '\0';
  while (value >= 0) {
    *p++ = "0123456789abcdef"[value%radix];
    value /= radix;
    if (value == 0) break;
  }

  if (!sign) {
    *p = '-';
  }
  else {
    p--;
  }

  while (p >= reverse) {
    *str++ = *p--;
  }

  return str;
}

char *uitoa(uint32_t value, char *str, int radix) {
  char reverse[36];
  char *p = reverse;

  *p++ = '\0';
  while (value != 0) {
    *p++ = "0123456789abcdef"[value%radix];
    value /= radix;
    if (value == 0) break;
  }
  p--;

  while (p >= reverse) {
    *str++ = *p--;
  }

  return str;
}

#endif
