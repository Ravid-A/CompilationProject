public int foo(args>> int: x, y, z; float: f){
    var int: b <- 3+5, c <- 4, d;
    var float: e;

    b[3] <- 3;

    *b <- 3;

    return b;
}
private int goo(args>> int: x): static{ 
    foo(x, 2, 3, 4.0);
    return 3;
}

public void barr(args>> int: i, j, k): static
{
    var double: max, min;
    private bool fee(args>> int: l, m, n; float: x, y)
    {
        return true;
    }
}