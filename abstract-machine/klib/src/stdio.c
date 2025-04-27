#include <am.h>
#include <klib.h>
#include <klib-macros.h>
#include <stdarg.h>

#if !defined(__ISA_NATIVE__) || defined(__NATIVE_USE_KLIB__)

int printf(const char *fmt, ...) {
  char out[1024];
  va_list ap;
  va_start(ap, fmt);
  int length = vsprintf(out, fmt, ap);
  va_end(ap);
  putstr(out);
  return length;
}

int vsprintf(char *out, const char *fmt, va_list ap) {
  char *p;
  va_list p_next_arg = ap;

  for (p = out; *fmt; fmt++) {
    if (*fmt != '%'){
        *p++ = *fmt;
        continue;
    }
    fmt++;
    switch (*fmt) {
      case 'd':
          itoa(va_arg(p_next_arg, int), p, 10);
          p += strlen(p);
          break;
      case 'x':
          uitoa(va_arg(p_next_arg, unsigned int), p, 16);
          p += strlen(p);
          break;
      case 'c':
          *p++ = va_arg(p_next_arg, int);  // 自动char 转换 int
          break;
      case 's':
          *p = '\0';
          strcat(p, va_arg(p_next_arg, char *));
          p += strlen(p);
          break;
      default:
          break;
    }
  }
  *p = '\0';
  return (p - out);
}

int sprintf(char *out, const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  int length = vsprintf(out, fmt, ap);
  va_end(ap);
  return length;
}

int snprintf(char *out, size_t n, const char *fmt, ...) {
  panic("Not implemented");
}

int vsnprintf(char *out, size_t n, const char *fmt, va_list ap) {
  panic("Not implemented");
}

#endif
