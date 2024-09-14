// Copyright (c) 2024, Peter Ohler, All rights reserved.

//go:build linux

package net

import (
	"syscall"
	"time"

	"github.com/ohler55/slip"
)

var domainMap = map[slip.Symbol]int{
	slip.Symbol(":alg"):        syscall.AF_ALG,
	slip.Symbol(":appletalk"):  syscall.AF_APPLETALK,
	slip.Symbol(":ash"):        syscall.AF_ASH,
	slip.Symbol(":atmpvc"):     syscall.AF_ATMPVC,
	slip.Symbol(":atmsvc"):     syscall.AF_ATMSVC,
	slip.Symbol(":ax25"):       syscall.AF_AX25,
	slip.Symbol(":bluetooth"):  syscall.AF_BLUETOOTH,
	slip.Symbol(":bridge"):     syscall.AF_BRIDGE,
	slip.Symbol(":caif"):       syscall.AF_CAIF,
	slip.Symbol(":can"):        syscall.AF_CAN,
	slip.Symbol(":decnet"):     syscall.AF_DECnet,
	slip.Symbol(":econet"):     syscall.AF_ECONET,
	slip.Symbol(":file"):       syscall.AF_FILE,
	slip.Symbol(":ieee802154"): syscall.AF_IEEE802154,
	slip.Symbol(":inet"):       syscall.AF_INET,
	slip.Symbol(":inet6"):      syscall.AF_INET6,
	slip.Symbol(":ipx"):        syscall.AF_IPX,
	slip.Symbol(":irda"):       syscall.AF_IRDA,
	slip.Symbol(":isdn"):       syscall.AF_ISDN,
	slip.Symbol(":iucv"):       syscall.AF_IUCV,
	slip.Symbol(":key"):        syscall.AF_KEY,
	slip.Symbol(":llc"):        syscall.AF_LLC,
	slip.Symbol(":local"):      syscall.AF_LOCAL,
	slip.Symbol(":max"):        syscall.AF_MAX,
	slip.Symbol(":netbeui"):    syscall.AF_NETBEUI,
	slip.Symbol(":netlink"):    syscall.AF_NETLINK,
	slip.Symbol(":netrom"):     syscall.AF_NETROM,
	// slip.Symbol(":nfc"):        syscall.AF_NFC,
	slip.Symbol(":packet"):   syscall.AF_PACKET,
	slip.Symbol(":phonet"):   syscall.AF_PHONET,
	slip.Symbol(":pppox"):    syscall.AF_PPPOX,
	slip.Symbol(":rds"):      syscall.AF_RDS,
	slip.Symbol(":rose"):     syscall.AF_ROSE,
	slip.Symbol(":route"):    syscall.AF_ROUTE,
	slip.Symbol(":rxrpc"):    syscall.AF_RXRPC,
	slip.Symbol(":security"): syscall.AF_SECURITY,
	slip.Symbol(":sna"):      syscall.AF_SNA,
	slip.Symbol(":tipc"):     syscall.AF_TIPC,
	slip.Symbol(":unix"):     syscall.AF_UNIX,
	slip.Symbol(":unspec"):   syscall.AF_UNSPEC,
	// slip.Symbol(":vsock"):      syscall.AF_VSOCK,
	slip.Symbol(":wanpipe"): syscall.AF_WANPIPE,
	slip.Symbol(":x25"):     syscall.AF_X25,
}

var typeMap = map[slip.Symbol]int{
	slip.Symbol(":dgram"):     syscall.SOCK_DGRAM,
	slip.Symbol(":nonblock"):  syscall.SOCK_NONBLOCK,
	slip.Symbol(":packet"):    syscall.SOCK_PACKET,
	slip.Symbol(":raw"):       syscall.SOCK_RAW,
	slip.Symbol(":rdm"):       syscall.SOCK_RDM,
	slip.Symbol(":seqpacket"): syscall.SOCK_SEQPACKET,
	slip.Symbol(":stream"):    syscall.SOCK_STREAM,
}

var protocolMap = map[slip.Symbol]int{
	slip.Symbol(":ah"): syscall.IPPROTO_AH,
	// slip.Symbol(":beetph"):   syscall.IPPROTO_BEETPH,
	slip.Symbol(":comp"):     syscall.IPPROTO_COMP,
	slip.Symbol(":dccp"):     syscall.IPPROTO_DCCP,
	slip.Symbol(":dstopts"):  syscall.IPPROTO_DSTOPTS,
	slip.Symbol(":egp"):      syscall.IPPROTO_EGP,
	slip.Symbol(":encap"):    syscall.IPPROTO_ENCAP,
	slip.Symbol(":esp"):      syscall.IPPROTO_ESP,
	slip.Symbol(":fragment"): syscall.IPPROTO_FRAGMENT,
	slip.Symbol(":gre"):      syscall.IPPROTO_GRE,
	slip.Symbol(":hopopts"):  syscall.IPPROTO_HOPOPTS,
	slip.Symbol(":icmp"):     syscall.IPPROTO_ICMP,
	slip.Symbol(":icmpv6"):   syscall.IPPROTO_ICMPV6,
	slip.Symbol(":idp"):      syscall.IPPROTO_IDP,
	slip.Symbol(":igmp"):     syscall.IPPROTO_IGMP,
	slip.Symbol(":ip"):       syscall.IPPROTO_IP,
	slip.Symbol(":ipip"):     syscall.IPPROTO_IPIP,
	slip.Symbol(":ipv6"):     syscall.IPPROTO_IPV6,
	// slip.Symbol(":mh"):       syscall.IPPROTO_MH,
	slip.Symbol(":mtp"):     syscall.IPPROTO_MTP,
	slip.Symbol(":none"):    syscall.IPPROTO_NONE,
	slip.Symbol(":pim"):     syscall.IPPROTO_PIM,
	slip.Symbol(":pup"):     syscall.IPPROTO_PUP,
	slip.Symbol(":raw"):     syscall.IPPROTO_RAW,
	slip.Symbol(":routing"): syscall.IPPROTO_ROUTING,
	slip.Symbol(":rsvp"):    syscall.IPPROTO_RSVP,
	slip.Symbol(":sctp"):    syscall.IPPROTO_SCTP,
	slip.Symbol(":tcp"):     syscall.IPPROTO_TCP,
	slip.Symbol(":tp"):      syscall.IPPROTO_TP,
	slip.Symbol(":udp"):     syscall.IPPROTO_UDP,
	slip.Symbol(":udplite"): syscall.IPPROTO_UDPLITE,
}

type FdSet syscall.FdSet

func Select(r, w, e *FdSet, timeout time.Duration) error {
	var (
		high int
		tvp  *syscall.Timeval
	)
	if 0 <= timeout {
		tv := syscall.NsecToTimeval(int64(timeout))
		tvp = &tv
	}
	for _, fs := range []*FdSet{r, w, e} {
		if fs != nil {
			hi := fs.Highest()
			if high < hi {
				high = hi
			}
		}
	}
	_, err := syscall.Select(high+1, (*syscall.FdSet)(r), (*syscall.FdSet)(w), (*syscall.FdSet)(e), tvp)

	return err
}

func (fs *FdSet) Set(fd int) {
	fs.Bits[fd/64] |= 0x01 << (fd % 64)
}

func (fs *FdSet) IsSet(fd int) bool {
	return (fs.Bits[fd/3264] & (0x01 << (fd % 64))) != 0
}

func (fs *FdSet) Highest() (high int) {
	var n int
	for i, b := range fs.Bits {
		if b != 0 {
			for j := 63; 0 <= j; j-- {
				if (b & (0x01 << j)) != 0 {
					n = i*64 + j
					if high < n {
						high = n
					}
					break
				}
			}
		}
	}
	return
}
