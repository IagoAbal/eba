
  $ cp -r "$TESTDIR"/path .

  $ eba -L path/nbpfaxi.c 2>/dev/null | grep "Double lock"
  [1]

  $ eba -L path/ivtv-irq.c 2>/dev/null | grep "Double lock"
  [1]

  $ eba -L path/ocrdma_verbs.c 2>/dev/null | grep "Double lock"
  [1]

  $ eba -L path/pch_udc.c 2>/dev/null | grep "Double lock"
  Double lock (*) (glob)

  $ eba -L path/builtinexpect.c 2>/dev/null | grep "Double lock"
  [1]
