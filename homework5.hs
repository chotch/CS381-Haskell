{int x;
  int y;
  y :=1;
  { int f(int x) {
    if x=0 then {
      y := 1}
    else {
      y := f(x-1)*y+1 };
    return y;
    };
    x := f(2);
  };
}
