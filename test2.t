private void goo(): static { 
}

/* scope 0: */
public void main(): static
{
    private void foo1()
    {
    }

    public void goo2()
    {
        
        public void goo3()
        {
            main();
        }
        foo1();
    }

    goo();
}