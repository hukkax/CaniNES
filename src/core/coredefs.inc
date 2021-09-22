// enable C-style operators
{$COperators on}

// remove annoying "Case statement does not handle all possible cases" warning
{$if fpc_fullversion >= 030300} {$warn 6060 off} {$endif}
