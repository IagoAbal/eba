
  $ cp -r "$TESTDIR"/heuristics .

  $ eba -L --ignore-writes heuristics/lockarray1.c 2>/dev/null | grep "Double lock"
  Double lock (*) (glob)

  $ eba -L heuristics/lockarray1.c 2>/dev/null | grep "Double lock"
  [1]
