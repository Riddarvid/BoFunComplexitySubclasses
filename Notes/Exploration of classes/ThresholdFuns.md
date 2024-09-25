
An iterated threshold fun is defined as:

IteratedThresholdFun f = Free ThresholdFun f

Which in turn means

Pure f | Free (ThresholdFun IteratedThresholdFun)

SO either we simply have a single function f, or we have a threshold function whose sub-functions are threshold functions, thus creating the iterative property.

Further, we have 

type IteratedThresholdFun' = IteratedThresholdFun ()

Giving us 

Pure () | Free (ThresholdFun IteratedThresholdFun')

which is isomorphic with

Maybe (ThresholdFun IteratedThresholdFun')

------------------------------------------------

Vi får 7 threshold funs med vår gamla metod, men 5 med den nya för 3 bitar. Varför?

1 -> Pure () = id
1 1-bits funktion

0 0
1 1
ID

2 -> [1,1]
(1,2), (2,1)
[id, id]
2 2-bits funktioner

00 0
01 1
10 1
11 1
OR

00 0
01 0
10 0
11 1
AND

3 -> [1,1,1], [1, 2]

[1,1,1] ->
(1,3), (2,2), (3,1)
[id,id,id]

000 0
001 1
010 1
011 1
100 1
101 1
110 1
111 1
OR3

000 0
001 0
010 0
011 1
100 0
101 1
110 1
111 1

000 0
001 0
010 0
011 0
100 0
101 0
110 0
111 1
AND3

[1,2] ->
(1,2), (2,1)
[id, OR], [id, AND]
3 + 2\*2 = 7 3-bits funktioner

000 0
001 1
010 1
011 1
100 1
101 1
110 1
111 1
OR3 - dublett

000 0
001 0
010 0
011 0
100 0
101 1
110 1
111 1

000 0
001 0
010 0
011 1
100 1
101 1
110 1
111 1

000 0
001 0
010 0
011 0
100 0
101 0
110 0
111 1
AND3 - dublett