Generates Fibonacci numbers based on a decimal input.
There is no input error checking, so make sure you only enter digits and a
newline!

Store -38 (start of ASCII numbers relative to newline) in slot 2
{*}==========} {*}==========} {={o}
{*}==========} {={o}
{*}========} {={o}
{o}==} {====={o}

Read input, subtract 10 (newline)
{========={o} {*}==========} {={o}

Start loop
{==={*}

    Subcounter to multiply by 10
    {o}==========}
    
    {==={*}
        Load counter and temp counter, then add
        {o}} {======{o} {o}=} {======{o} {={o}
        
        Store in temp counter
        {o}=} {====={o}
        
        Subtract 1 from subcounter
        {*}=} {={o}
    {===={*} {{o}
    
    Load -38, add to ASCII value
    {o}==} {======{o} {={o}
    
    Load temp counter, add to new number, store in counter
    {o}=} {======{o} {={o} {o}} {====={o}

    Read input, subtract 10 (newline)
    {========={o} {*}==========} {={o}

End loop 
{===={*}

Newline
{o}==========} {========{o}

If counter is 0, exit
    Store 1 in slot 1
    {o}=} {o}=} {====={o}

    Load counter
    {o}} {======{o}

    If counter non-zero, set slot 1 to 0
    {==={o} {o}} {o}=} {====={o} {o}} {===={o}

    Load slot 1
    {o}=} {======{o}

    If slot 1 is 1, quit
    {==={o} {=========={o} {===={o}
End if

Print 0, newline
{o}} {======={o}
{o}==========} {========{o}

If counter is 1, exit
    Store 1 in slot 1
    {o}=} {o}=} {====={o}

    Load counter and subtract 1
    {o}} {======{o} {*}=} {={o}

    If counter non-zero, set slot 1 to 0
    {==={o} {o}} {o}=} {====={o} {o}} {===={o}

    Load slot 1
    {o}=} {======{o}

    If slot 1 is 1, quit
    {==={o} {=========={o} {===={o}
End if

Print 1 and newline
{o}=} {======={o}
{o}==========} {========{o}

Store first two numbers (0, 1) in locations (1, 2)
{o}} {o}=} {====={o} {o}=} {o}==} {====={o}

Load counter, subtract 2, store and pop only the location
{o}} {======{o} {*}==} {={o} {o}} {====={*} {{o}

Start loop
{==={o}

    Load top two numbers
    {o}=} {======{o} {o}==} {======{o}

	Add top two numbers without popping them and print new number
	{={*} {======={*}
    
    Store the new top two numbers and pop the third
    {o}==} {====={o}
    {o}=} {====={o}
    {{o}
    
    Print newline
    {o}==========} {========{o}
	
	Load counter, subtract 1, store and pop only the location
	{o}} {======{o} {*}=} {={o} {o}} {====={*} {{o} 

End loop
{===={o}