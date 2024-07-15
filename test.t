public int foo(args>> int: x, y, z; float: f){
    var int: b;

    if (x > y) {
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