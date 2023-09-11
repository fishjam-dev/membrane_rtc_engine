#include "lib.h"

const char *bunch_errno_string() {
  switch (errno) {
#ifdef E2BIG
  case E2BIG:
    return "e2big";
#endif
#ifdef EACCES
  case EACCES:
    return "eacces";
#endif
#ifdef EADDRINUSE
  case EADDRINUSE:
    return "eaddrinuse";
#endif
#ifdef EADDRNOTAVAIL
  case EADDRNOTAVAIL:
    return "eaddrnotavail";
#endif
#ifdef EAFNOSUPPORT
  case EAFNOSUPPORT:
    return "eafnosupport";
#endif
#ifdef EAGAIN
  case EAGAIN:
    return "eagain";
#endif
#ifdef EALREADY
  case EALREADY:
    return "ealready";
#endif
#ifdef EBADE
  case EBADE:
    return "ebade";
#endif
#ifdef EBADF
  case EBADF:
    return "ebadf";
#endif
#ifdef EBADFD
  case EBADFD:
    return "ebadfd";
#endif
#ifdef EBADMSG
  case EBADMSG:
    return "ebadmsg";
#endif
#ifdef EBADR
  case EBADR:
    return "ebadr";
#endif
#ifdef EBADRQC
  case EBADRQC:
    return "ebadrqc";
#endif
#ifdef EBADSLT
  case EBADSLT:
    return "ebadslt";
#endif
#ifdef EBUSY
  case EBUSY:
    return "ebusy";
#endif
#ifdef ECANCELED
  case ECANCELED:
    return "ecanceled";
#endif
#ifdef ECHILD
  case ECHILD:
    return "echild";
#endif
#ifdef ECHRNG
  case ECHRNG:
    return "echrng";
#endif
#ifdef ECOMM
  case ECOMM:
    return "ecomm";
#endif
#ifdef ECONNABORTED
  case ECONNABORTED:
    return "econnaborted";
#endif
#ifdef ECONNREFUSED
  case ECONNREFUSED:
    return "econnrefused";
#endif
#ifdef ECONNRESET
  case ECONNRESET:
    return "econnreset";
#endif
#if defined(EDEADLK) && (!defined(EWOULDBLOCK) || (EDEADLK != EWOULDBLOCK))
  case EDEADLK:
    return "edeadlk";
#endif
#if defined(EDEADLOCK) && (!defined(EDEADLK) || (EDEADLOCK != EDEADLK))
  case EDEADLOCK:
    return "edeadlock";
#endif
#ifdef EDESTADDRREQ
  case EDESTADDRREQ:
    return "edestaddrreq";
#endif
#ifdef EDOM
  case EDOM:
    return "edom";
#endif
#ifdef EDQUOT
  case EDQUOT:
    return "edquot";
#endif
#ifdef EEXIST
  case EEXIST:
    return "eexist";
#endif
#ifdef EFAULT
  case EFAULT:
    return "efault";
#endif
#ifdef EFBIG
  case EFBIG:
    return "efbig";
#endif
#ifdef EHOSTDOWN
  case EHOSTDOWN:
    return "ehostdown";
#endif
#ifdef EHOSTUNREACH
  case EHOSTUNREACH:
    return "ehostunreach";
#endif
#ifdef EHWPOISON
  case EHWPOISON:
    return "ehwpoison";
#endif
#ifdef EIDRM
  case EIDRM:
    return "eidrm";
#endif
#ifdef EILSEQ
  case EILSEQ:
    return "eilseq";
#endif
#ifdef EINPROGRESS
  case EINPROGRESS:
    return "einprogress";
#endif
#ifdef EINTR
  case EINTR:
    return "eintr";
#endif
#ifdef EINVAL
  case EINVAL:
    return "einval";
#endif
#ifdef EIO
  case EIO:
    return "eio";
#endif
#ifdef EISCONN
  case EISCONN:
    return "eisconn";
#endif
#ifdef EISDIR
  case EISDIR:
    return "eisdir";
#endif
#ifdef EISNAM
  case EISNAM:
    return "eisnam";
#endif
#ifdef EKEYEXPIRED
  case EKEYEXPIRED:
    return "ekeyexpired";
#endif
#ifdef EKEYREJECTED
  case EKEYREJECTED:
    return "ekeyrejected";
#endif
#ifdef EKEYREVOKED
  case EKEYREVOKED:
    return "ekeyrevoked";
#endif
#ifdef EL2HLT
  case EL2HLT:
    return "el2hlt";
#endif
#ifdef EL2NSYNC
  case EL2NSYNC:
    return "el2nsync";
#endif
#ifdef EL3HLT
  case EL3HLT:
    return "el3hlt";
#endif
#ifdef EL3RST
  case EL3RST:
    return "el3rst";
#endif
#ifdef ELIBACC
  case ELIBACC:
    return "elibacc";
#endif
#ifdef ELIBBAD
  case ELIBBAD:
    return "elibbad";
#endif
#ifdef ELIBMAX
  case ELIBMAX:
    return "elibmax";
#endif
#ifdef ELIBSCN
  case ELIBSCN:
    return "elibscn";
#endif
#ifdef ELIBEXEC
  case ELIBEXEC:
    return "elibexec";
#endif
#ifdef ELNRANGE
  case ELNRANGE:
    return "elnrange";
#endif
#ifdef ELOOP
  case ELOOP:
    return "eloop";
#endif
#ifdef EMEDIUMTYPE
  case EMEDIUMTYPE:
    return "emediumtype";
#endif
#ifdef EMFILE
  case EMFILE:
    return "emfile";
#endif
#ifdef EMLINK
  case EMLINK:
    return "emlink";
#endif
#ifdef EMSGSIZE
  case EMSGSIZE:
    return "emsgsize";
#endif
#ifdef EMULTIHOP
  case EMULTIHOP:
    return "emultihop";
#endif
#ifdef ENAMETOOLONG
  case ENAMETOOLONG:
    return "enametoolong";
#endif
#ifdef ENETDOWN
  case ENETDOWN:
    return "enetdown";
#endif
#ifdef ENETRESET
  case ENETRESET:
    return "enetreset";
#endif
#ifdef ENETUNREACH
  case ENETUNREACH:
    return "enetunreach";
#endif
#ifdef ENFILE
  case ENFILE:
    return "enfile";
#endif
#ifdef ENOANO
  case ENOANO:
    return "enoano";
#endif
#ifdef ENOBUFS
  case ENOBUFS:
    return "enobufs";
#endif
#ifdef ENODATA
  case ENODATA:
    return "enodata";
#endif
#ifdef ENODEV
  case ENODEV:
    return "enodev";
#endif
#ifdef ENOENT
  case ENOENT:
    return "enoent";
#endif
#ifdef ENOEXEC
  case ENOEXEC:
    return "enoexec";
#endif
#ifdef ENOKEY
  case ENOKEY:
    return "enokey";
#endif
#ifdef ENOLCK
  case ENOLCK:
    return "enolck";
#endif
#ifdef ENOLINK
  case ENOLINK:
    return "enolink";
#endif
#ifdef ENOMEDIUM
  case ENOMEDIUM:
    return "enomedium";
#endif
#ifdef ENOMEM
  case ENOMEM:
    return "enomem";
#endif
#ifdef ENOMSG
  case ENOMSG:
    return "enomsg";
#endif
#ifdef ENONET
  case ENONET:
    return "enonet";
#endif
#ifdef ENOPKG
  case ENOPKG:
    return "enopkg";
#endif
#ifdef ENOPROTOOPT
  case ENOPROTOOPT:
    return "enoprotoopt";
#endif
#ifdef ENOSPC
  case ENOSPC:
    return "enospc";
#endif
#ifdef ENOSR
  case ENOSR:
    return "enosr";
#endif
#ifdef ENOSTR
  case ENOSTR:
    return "enostr";
#endif
#ifdef ENOSYS
  case ENOSYS:
    return "enosys";
#endif
#ifdef ENOTBLK
  case ENOTBLK:
    return "enotblk";
#endif
#ifdef ENOTCONN
  case ENOTCONN:
    return "enotconn";
#endif
#ifdef ENOTDIR
  case ENOTDIR:
    return "enotdir";
#endif
#ifdef ENOTEMPTY
  case ENOTEMPTY:
    return "enotempty";
#endif
#ifdef ENOTRECOVERABLE
  case ENOTRECOVERABLE:
    return "enotrecoverable";
#endif
#ifdef ENOTSOCK
  case ENOTSOCK:
    return "enotsock";
#endif
#ifdef ENOTSUP
  case ENOTSUP:
    return "enotsup";
#endif
#ifdef ENOTTY
  case ENOTTY:
    return "enotty";
#endif
#ifdef ENOTUNIQ
  case ENOTUNIQ:
    return "enotuniq";
#endif
#ifdef ENXIO
  case ENXIO:
    return "enxio";
#endif
#if defined(EOPNOTSUPP) && (!defined(ENOTSUP) || (EOPNOTSUPP != ENOTSUP))
  case EOPNOTSUPP:
    return "eopnotsupp";
#endif
#ifdef EOVERFLOW
  case EOVERFLOW:
    return "eoverflow";
#endif
#ifdef EOWNERDEAD
  case EOWNERDEAD:
    return "eownerdead";
#endif
#ifdef EPERM
  case EPERM:
    return "eperm";
#endif
#ifdef EPFNOSUPPORT
  case EPFNOSUPPORT:
    return "epfnosupport";
#endif
#ifdef EPIPE
  case EPIPE:
    return "epipe";
#endif
#ifdef EPROTO
  case EPROTO:
    return "eproto";
#endif
#ifdef EPROTONOSUPPORT
  case EPROTONOSUPPORT:
    return "eprotonosupport";
#endif
#ifdef EPROTOTYPE
  case EPROTOTYPE:
    return "eprototype";
#endif
#ifdef ERANGE
  case ERANGE:
    return "erange";
#endif
#ifdef EREMCHG
  case EREMCHG:
    return "eremchg";
#endif
#ifdef EREMOTE
  case EREMOTE:
    return "eremote";
#endif
#ifdef EREMOTEIO
  case EREMOTEIO:
    return "eremoteio";
#endif
#ifdef ERESTART
  case ERESTART:
    return "erestart";
#endif
#ifdef ERFKILL
  case ERFKILL:
    return "erfkill";
#endif
#ifdef EROFS
  case EROFS:
    return "erofs";
#endif
#ifdef ESHUTDOWN
  case ESHUTDOWN:
    return "eshutdown";
#endif
#ifdef ESPIPE
  case ESPIPE:
    return "espipe";
#endif
#ifdef ESOCKTNOSUPPORT
  case ESOCKTNOSUPPORT:
    return "esocktnosupport";
#endif
#ifdef ESRCH
  case ESRCH:
    return "esrch";
#endif
#ifdef ESTALE
  case ESTALE:
    return "estale";
#endif
#ifdef ESTRPIPE
  case ESTRPIPE:
    return "estrpipe";
#endif
#ifdef ETIME
  case ETIME:
    return "etime";
#endif
#ifdef ETIMEDOUT
  case ETIMEDOUT:
    return "etimedout";
#endif
#ifdef ETOOMANYREFS
  case ETOOMANYREFS:
    return "etoomanyrefs";
#endif
#ifdef ETXTBSY
  case ETXTBSY:
    return "etxtbsy";
#endif
#ifdef EUCLEAN
  case EUCLEAN:
    return "euclean";
#endif
#ifdef EUNATCH
  case EUNATCH:
    return "eunatch";
#endif
#ifdef EUSERS
  case EUSERS:
    return "eusers";
#endif
#if defined(EWOULDBLOCK) && (!defined(EAGAIN) || (EWOULDBLOCK != EAGAIN))
  case EWOULDBLOCK:
    return "ewouldblock";
#endif
#ifdef EXDEV
  case EXDEV:
    return "exdev";
#endif
#ifdef EXFULL
  case EXFULL:
    return "exfull";
#endif
  }
  return "unknown";
}
