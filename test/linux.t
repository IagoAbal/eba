
  $ cp -r "$TESTDIR"/linux .

  $ eba -L linux/cmt_speech.i 2>/dev/null | grep -A 2 "Double lock"
  Double lock (*) (glob)
  first at drivers/hsi/clients/cmt_speech.c:443
  second at drivers/hsi/clients/cmt_speech.c:407

  $ eba -L linux/dquot.i 2>/dev/null | grep -A 2 "Double lock"
  Double lock (*) (glob)
  first at fs/quota/dquot.c:903
  second at fs/quota/dquot.c:1523

  $ eba -L linux/htt_rx.i 2>/dev/null | grep -A 2 "Double lock"
  Double lock (*) (glob)
  first at drivers/net/wireless/ath/ath10k/htt_rx.c:2458
  second at drivers/net/wireless/ath/ath10k/htt_rx.c:182

  $ eba -L linux/pch_udc.i 2>/dev/null | egrep -B 2 "second.*2694"
  Double lock (*) (glob)
  first at drivers/usb/gadget/udc/pch_udc.c:2791
  second at drivers/usb/gadget/udc/pch_udc.c:2694

