public void foo(args>> int: x, y, z; float: f){
    if (x > y) {
        x <- x + y;
    }
    else {
        y <- x + y + z;
        z <- y * 2;
        f <- z;
    }
}
private char goo(): static{
    return 'a';
}