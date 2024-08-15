public int foo(args>> double: x): static{
    var int: a, b;
    a <- 23;
    if (a == 23)
        b <- 10;
    else
        b <- 19;
    return b + a;
}

public void main(): static {
    var int: a, b;
    string d[30] <- "asfliahjsg";
    a <- 0;
    while(a < 10) {
        b <- foo(3.14);
        a <- a + b;
    }      
}