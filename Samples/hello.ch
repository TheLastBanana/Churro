Hello World, based on a Brainfuck program:
http://en.wikipedia.org/wiki/Brainfuck#Examples

Push 10 to stack
{o}==========}

Start loop, keeping 10 on stack
{==={*}

    Load cell 0, add 1, store in cell 0
    {o}} {======{o} {o}=} {={o} {o}} {====={o}

    Load cell 1, add 3, store in cell 1
    {o}=} {======{o} {o}===} {={o} {o}=} {====={o}

    Load cell 2, add 7, store in cell 2
    {o}==} {======{o} {o}=======} {={o} {o}==} {====={o}

    Load cell 3, add 9, store in cell 3
    {o}===} {======{o} {o}=========} {={o} {o}===} {====={o}

    Load cell 4, add 10, store in cell 4
    {o}====} {======{o} {o}==========} {={o} {o}====} {====={o}

    Add -1 to counter
    {*}=} {={o}

End loop when counter == 0, then pop the counter
{===={*} {{o}

Load cell 2 (70) and add 2, then print (H)
{o}==} {======{o} {o}==} {={o} {========{o}

Load cell 4 (100) and add 1, then print (e), keeping value on stack
{o}====} {======{o} {o}=} {={o} {========{*}

Add 7, then print twice (l)
{o}=======} {={o} {========{*} {========{*}

Add 3, then print (o), keeping value on stack
{o}===} {={o} {========{*}

Load cell 1 (30) and add 2, then print (space)
{o}=} {======{o} {o}==} {={o} {========{o}

Load cell 3 (90) and add -3, then print (W)
{o}===} {======{o} {*}===} {={o} {========{o}

Print the value on stack (o) and leave it there
{========{*}

Add 3, then print (r), keeping value on stack
{o}===} {={o} {========{*}

Add -6, then print (l), keeping value on stack
{*}======} {={o} {========{*}

Add -8, then print (d), keeping value on stack
{*}========} {={o} {========{*}

Load cell 1 (30) and add 3, then print (!)
{o}=} {======{o} {o}===} {={o} {========{o}

Load cell 0 (10), then print (newline)
{o}} {======{o} {========{*}