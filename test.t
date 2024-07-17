public int foo(args>> int: x, y, z; float: f){
    var int: b <- 3+5, c <- 4, d;
    var float: e;

    if (x > y && true || !x) {
        x <- x + y;
        goo(x);
    }
    else {
        y <- x + y + z;
        z <- y * 2;
        f <- z;
    }

    b <- x + y + z;

    return b;
}
private int goo(args>> int: x): static{ 
    foo(x, 2, 3, 4.0);
    return 3;
}

public int barr(args>> int: i, j, k): static
{
    var double: max, min;
    private bool fee(args>> int: l, m, n; float: x, y)
    {
        return true;
    }
    return 0;
}