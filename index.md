EBA is a prototype tool to find non-trivial resource manipulation bugs in C programs, at compile-time, and super-fast.

In its few months of existence, EBA has found several double-lock bugs in Linux 4.7--4.10 releases (i.e. in code that has passed code reviews). All the following bugs are caught by EBA in a matter of _seconds_:

- [HSI: cmt_speech: Fix double spin_lock](https://github.com/torvalds/linux/commit/3c13ab1d96e1924ef73b1a20c1ccccc993b6fb58)
- [usb: gadget: pch_udc: reorder spin_[un]lock to avoid deadlock](https://github.com/torvalds/linux/commit/1d23d16a88e6c8143b07339435ba061b131ebb8c)
- [ath10k: fix deadlock while processing rx_in_ord_ind](https://patchwork.kernel.org/patch/9166323/) [1]
- [net: ethernet: ti: cpdma: fix lockup in cpdma_ctlr_destroy()](https://github.com/torvalds/linux/commit/fccd5badb84de03fef9b072e7ae72fe0ea8348e3) [2]
- [libceph: ceph_build_auth() doesn't need ceph_auth_build_hello()](https://github.com/torvalds/linux/commit/464691bd52b46a565153ec2a3b8b9984dacd4a00)
- [[PATCH] Fix: scsi: megaraid: reduce the scope of pending-list lock to avoid double lock](http://www.spinics.net/lists/linux-scsi/msg100996.html)
- [iommu/vt-d: Fix dead-locks in disable_dmar_iommu() path](https://github.com/torvalds/linux/commit/bea64033dd7b5fb6296eda8266acab6364ce1554) [3]
- [Re: Potential double-lock BUG in drivers/tty/serial/sh-sci.c (Linux 4.9)](http://www.spinics.net/lists/linux-serial/msg24393.html)
- [Potential deadlock BUG in drivers/net/wireless/st/cw1200/sta.c (Linux 4.9)](https://www.mail-archive.com/netdev@vger.kernel.org/msg138296.html) [4]
- [Potential deadlock BUG in Linux 4.9 drivers/dma/coh901318.c](http://www.spinics.net/lists/arm-kernel/msg543590.html) [4]
- [[PATCH] [media] pctv452e: fix double lock bug](http://www.spinics.net/lists/linux-media/msg108700.html) [4]
- [Potential double-lock BUG in drivers/infiniband/core/umem_odp.c (Linux 4.9-rc7)](http://www.spinics.net/lists/linux-rdma/msg43736.html) [4]
- [dmaengine: pl330: fix double lock](https://github.com/torvalds/linux/commit/91539eb1fda2d530d3b268eef542c5414e54bf1a)
- [cros_ec: Fix deadlock when EC is not responsive at probe](https://github.com/torvalds/linux/commit/d4da97e59e1004aa1a15dd75469def20cd84ab99) [3]
- [dmaengine: coh901318: Fix a double-lock bug](https://github.com/torvalds/linux/commit/627469e4445b9b12e0229b3bdf8564d5ce384dd7), note that [I reported that bug and sketched the bug-fix two years ealier](https://www.spinics.net/lists/arm-kernel/msg543590.html).

[1] I reported this bug in private email communication with Kalle Valo, but I was slow and the bug had been found and fixed already.

[2] [I was slow at reporting this bug too.](https://www.spinics.net/lists/linux-omap/msg132214.html)

[3] For some reason I was not credited, but I reported this bug! Sometimes I reported [publicly](https://lists.linuxfoundation.org/pipermail/iommu/2016-September/018614.html) and sometimes privately by emailing the maintainers. 

[4] To be confirmed.

## Installation

EBA is open source: [https://github.com/iagoabal/eba](https://github.com/iagoabal/eba)

(There are installation instructions in the README.md file.)

## Foundations

EBA first uses side-effect analysis to build a program abstraction. This abstraction is later model checked in search of bug patterns.

- [A short 2-page paper about EBA (the evaluation results are outdated)](http://dl.iagoabal.eu/eba/short.pdf)
- [A research paper to appear in VMCAI 2017](http://dl.iagoabal.eu/eba/vmcai.pdf)
- [EBA's inference system for CIL](http://dl.iagoabal.eu/eba/cil.pdf)

## Author(s)

EBA is a tool by Iago Abal.

This project was originally part of my PhD at IT University of Copenhagen, where I worked together with [Andrzej Wąsowski](http://www.itu.dk/~wasowski/) and [Claus Brabrand](http://www.itu.dk/people/brabrand/).

