// Copyright (c) 2024, Peter Ohler, All rights reserved.

//go:build darwin

package net

import (
	"syscall"
	"time"

	"github.com/ohler55/slip"
)

var domainMap = map[slip.Symbol]int{
	slip.Symbol(":appletalk"):  syscall.AF_APPLETALK,
	slip.Symbol(":ccitt"):      syscall.AF_CCITT,
	slip.Symbol(":chaos"):      syscall.AF_CHAOS,
	slip.Symbol(":cnt"):        syscall.AF_CNT,
	slip.Symbol(":coip"):       syscall.AF_COIP,
	slip.Symbol(":datakit"):    syscall.AF_DATAKIT,
	slip.Symbol(":deCnet"):     syscall.AF_DECnet,
	slip.Symbol(":dli"):        syscall.AF_DLI,
	slip.Symbol(":e164"):       syscall.AF_E164,
	slip.Symbol(":ecma"):       syscall.AF_ECMA,
	slip.Symbol(":hylink"):     syscall.AF_HYLINK,
	slip.Symbol(":ieee80211"):  syscall.AF_IEEE80211,
	slip.Symbol(":implink"):    syscall.AF_IMPLINK,
	slip.Symbol(":inet"):       syscall.AF_INET,
	slip.Symbol(":inet6"):      syscall.AF_INET6,
	slip.Symbol(":ipx"):        syscall.AF_IPX,
	slip.Symbol(":isdn"):       syscall.AF_ISDN,
	slip.Symbol(":iso"):        syscall.AF_ISO,
	slip.Symbol(":lat"):        syscall.AF_LAT,
	slip.Symbol(":link"):       syscall.AF_LINK,
	slip.Symbol(":local"):      syscall.AF_LOCAL,
	slip.Symbol(":max"):        syscall.AF_MAX,
	slip.Symbol(":natm"):       syscall.AF_NATM,
	slip.Symbol(":ndrv"):       syscall.AF_NDRV,
	slip.Symbol(":netbios"):    syscall.AF_NETBIOS,
	slip.Symbol(":ns"):         syscall.AF_NS,
	slip.Symbol(":osi"):        syscall.AF_OSI,
	slip.Symbol(":ppp"):        syscall.AF_PPP,
	slip.Symbol(":pup"):        syscall.AF_PUP,
	slip.Symbol(":reserved36"): syscall.AF_RESERVED_36,
	slip.Symbol(":route"):      syscall.AF_ROUTE,
	slip.Symbol(":sip"):        syscall.AF_SIP,
	slip.Symbol(":sna"):        syscall.AF_SNA,
	slip.Symbol(":system"):     syscall.AF_SYSTEM,
	slip.Symbol(":unix"):       syscall.AF_UNIX,
	slip.Symbol(":unspec"):     syscall.AF_UNSPEC,
	slip.Symbol(":utun"):       syscall.AF_UTUN,
}

var typeMap = map[slip.Symbol]int{
	slip.Symbol(":datagram"):   syscall.SOCK_DGRAM,
	slip.Symbol(":maxaddrlen"): syscall.SOCK_MAXADDRLEN,
	slip.Symbol(":raw"):        syscall.SOCK_RAW,
	slip.Symbol(":rdm"):        syscall.SOCK_RDM,
	slip.Symbol(":seqpacket"):  syscall.SOCK_SEQPACKET,
	slip.Symbol(":stream"):     syscall.SOCK_STREAM,
}

var protocolMap = map[slip.Symbol]int{
	slip.Symbol(":3PC"):       syscall.IPPROTO_3PC,
	slip.Symbol(":adfs"):      syscall.IPPROTO_ADFS,
	slip.Symbol(":ah"):        syscall.IPPROTO_AH,
	slip.Symbol(":ahip"):      syscall.IPPROTO_AHIP,
	slip.Symbol(":apes"):      syscall.IPPROTO_APES,
	slip.Symbol(":argus"):     syscall.IPPROTO_ARGUS,
	slip.Symbol(":ax25"):      syscall.IPPROTO_AX25,
	slip.Symbol(":bha"):       syscall.IPPROTO_BHA,
	slip.Symbol(":blt"):       syscall.IPPROTO_BLT,
	slip.Symbol(":brsatmon"):  syscall.IPPROTO_BRSATMON,
	slip.Symbol(":cftp"):      syscall.IPPROTO_CFTP,
	slip.Symbol(":chaos"):     syscall.IPPROTO_CHAOS,
	slip.Symbol(":cmtp"):      syscall.IPPROTO_CMTP,
	slip.Symbol(":cphb"):      syscall.IPPROTO_CPHB,
	slip.Symbol(":cpnx"):      syscall.IPPROTO_CPNX,
	slip.Symbol(":ddp"):       syscall.IPPROTO_DDP,
	slip.Symbol(":dgp"):       syscall.IPPROTO_DGP,
	slip.Symbol(":divert"):    syscall.IPPROTO_DIVERT,
	slip.Symbol(":done"):      syscall.IPPROTO_DONE,
	slip.Symbol(":dstopts"):   syscall.IPPROTO_DSTOPTS,
	slip.Symbol(":egp"):       syscall.IPPROTO_EGP,
	slip.Symbol(":emcon"):     syscall.IPPROTO_EMCON,
	slip.Symbol(":encap"):     syscall.IPPROTO_ENCAP,
	slip.Symbol(":eon"):       syscall.IPPROTO_EON,
	slip.Symbol(":esp"):       syscall.IPPROTO_ESP,
	slip.Symbol(":etherip"):   syscall.IPPROTO_ETHERIP,
	slip.Symbol(":fragment"):  syscall.IPPROTO_FRAGMENT,
	slip.Symbol(":ggp"):       syscall.IPPROTO_GGP,
	slip.Symbol(":gmtp"):      syscall.IPPROTO_GMTP,
	slip.Symbol(":gre"):       syscall.IPPROTO_GRE,
	slip.Symbol(":hello"):     syscall.IPPROTO_HELLO,
	slip.Symbol(":hmp"):       syscall.IPPROTO_HMP,
	slip.Symbol(":hopopts"):   syscall.IPPROTO_HOPOPTS,
	slip.Symbol(":icmp"):      syscall.IPPROTO_ICMP,
	slip.Symbol(":icmpv6"):    syscall.IPPROTO_ICMPV6,
	slip.Symbol(":idp"):       syscall.IPPROTO_IDP,
	slip.Symbol(":idpr"):      syscall.IPPROTO_IDPR,
	slip.Symbol(":idrp"):      syscall.IPPROTO_IDRP,
	slip.Symbol(":igmp"):      syscall.IPPROTO_IGMP,
	slip.Symbol(":igp"):       syscall.IPPROTO_IGP,
	slip.Symbol(":igrp"):      syscall.IPPROTO_IGRP,
	slip.Symbol(":il"):        syscall.IPPROTO_IL,
	slip.Symbol(":inlsp"):     syscall.IPPROTO_INLSP,
	slip.Symbol(":inp"):       syscall.IPPROTO_INP,
	slip.Symbol(":ip"):        syscall.IPPROTO_IP,
	slip.Symbol(":ipcomp"):    syscall.IPPROTO_IPCOMP,
	slip.Symbol(":ipcv"):      syscall.IPPROTO_IPCV,
	slip.Symbol(":ipeip"):     syscall.IPPROTO_IPEIP,
	slip.Symbol(":ipip"):      syscall.IPPROTO_IPIP,
	slip.Symbol(":ippc"):      syscall.IPPROTO_IPPC,
	slip.Symbol(":ipV4"):      syscall.IPPROTO_IPV4,
	slip.Symbol(":ipV6"):      syscall.IPPROTO_IPV6,
	slip.Symbol(":irtp"):      syscall.IPPROTO_IRTP,
	slip.Symbol(":kryptolan"): syscall.IPPROTO_KRYPTOLAN,
	slip.Symbol(":larp"):      syscall.IPPROTO_LARP,
	slip.Symbol(":leaf1"):     syscall.IPPROTO_LEAF1,
	slip.Symbol(":leaf2"):     syscall.IPPROTO_LEAF2,
	slip.Symbol(":max"):       syscall.IPPROTO_MAX,
	slip.Symbol(":maxid"):     syscall.IPPROTO_MAXID,
	slip.Symbol(":meas"):      syscall.IPPROTO_MEAS,
	slip.Symbol(":mhrp"):      syscall.IPPROTO_MHRP,
	slip.Symbol(":micp"):      syscall.IPPROTO_MICP,
	slip.Symbol(":mtp"):       syscall.IPPROTO_MTP,
	slip.Symbol(":mux"):       syscall.IPPROTO_MUX,
	slip.Symbol(":nd"):        syscall.IPPROTO_ND,
	slip.Symbol(":nhrp"):      syscall.IPPROTO_NHRP,
	slip.Symbol(":none"):      syscall.IPPROTO_NONE,
	slip.Symbol(":nsp"):       syscall.IPPROTO_NSP,
	slip.Symbol(":nvpii"):     syscall.IPPROTO_NVPII,
	slip.Symbol(":ospfigp"):   syscall.IPPROTO_OSPFIGP,
	slip.Symbol(":pgm"):       syscall.IPPROTO_PGM,
	slip.Symbol(":pigp"):      syscall.IPPROTO_PIGP,
	slip.Symbol(":pim"):       syscall.IPPROTO_PIM,
	slip.Symbol(":prm"):       syscall.IPPROTO_PRM,
	slip.Symbol(":pup"):       syscall.IPPROTO_PUP,
	slip.Symbol(":pvp"):       syscall.IPPROTO_PVP,
	slip.Symbol(":raw"):       syscall.IPPROTO_RAW,
	slip.Symbol(":rccmon"):    syscall.IPPROTO_RCCMON,
	slip.Symbol(":rdp"):       syscall.IPPROTO_RDP,
	slip.Symbol(":routing"):   syscall.IPPROTO_ROUTING,
	slip.Symbol(":rsvp"):      syscall.IPPROTO_RSVP,
	slip.Symbol(":rvd"):       syscall.IPPROTO_RVD,
	slip.Symbol(":satexpak"):  syscall.IPPROTO_SATEXPAK,
	slip.Symbol(":satmon"):    syscall.IPPROTO_SATMON,
	slip.Symbol(":sccsp"):     syscall.IPPROTO_SCCSP,
	slip.Symbol(":sctp"):      syscall.IPPROTO_SCTP,
	slip.Symbol(":sdrp"):      syscall.IPPROTO_SDRP,
	slip.Symbol(":sep"):       syscall.IPPROTO_SEP,
	slip.Symbol(":srpc"):      syscall.IPPROTO_SRPC,
	slip.Symbol(":st"):        syscall.IPPROTO_ST,
	slip.Symbol(":svmtp"):     syscall.IPPROTO_SVMTP,
	slip.Symbol(":swipe"):     syscall.IPPROTO_SWIPE,
	slip.Symbol(":tcf"):       syscall.IPPROTO_TCF,
	slip.Symbol(":tcp"):       syscall.IPPROTO_TCP,
	slip.Symbol(":tp"):        syscall.IPPROTO_TP,
	slip.Symbol(":tpxx"):      syscall.IPPROTO_TPXX,
	slip.Symbol(":trunk1"):    syscall.IPPROTO_TRUNK1,
	slip.Symbol(":trunk2"):    syscall.IPPROTO_TRUNK2,
	slip.Symbol(":ttp"):       syscall.IPPROTO_TTP,
	slip.Symbol(":udp"):       syscall.IPPROTO_UDP,
	slip.Symbol(":vines"):     syscall.IPPROTO_VINES,
	slip.Symbol(":visa"):      syscall.IPPROTO_VISA,
	slip.Symbol(":vmtp"):      syscall.IPPROTO_VMTP,
	slip.Symbol(":wbexpak"):   syscall.IPPROTO_WBEXPAK,
	slip.Symbol(":wbmon"):     syscall.IPPROTO_WBMON,
	slip.Symbol(":wsn"):       syscall.IPPROTO_WSN,
	slip.Symbol(":xnet"):      syscall.IPPROTO_XNET,
	slip.Symbol(":xtp"):       syscall.IPPROTO_XTP,
}

var msgFlagMap = map[slip.Symbol]int{
	slip.Symbol(":dontroute"): syscall.MSG_DONTROUTE,
	slip.Symbol(":dontwait"):  syscall.MSG_DONTWAIT,
	slip.Symbol(":eor"):       syscall.MSG_EOR,
	slip.Symbol(":oob"):       syscall.MSG_OOB,
	slip.Symbol(":peek"):      syscall.MSG_PEEK,
	slip.Symbol(":waitall"):   syscall.MSG_WAITALL,
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
	return syscall.Select(high+1, (*syscall.FdSet)(r), (*syscall.FdSet)(w), (*syscall.FdSet)(e), tvp)
}

func (fs *FdSet) Set(fd int) {
	fs.Bits[fd/32] |= 0x01 << (fd % 32)
}

func (fs *FdSet) IsSet(fd int) bool {
	return (fs.Bits[fd/32] & (0x01 << (fd % 32))) != 0
}

func (fs *FdSet) Highest() (high int) {
	var n int
	for i, b := range fs.Bits {
		if b != 0 {
			for j := 31; 0 <= j; j-- {
				if (b & (0x01 << j)) != 0 {
					n = i*32 + j
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
