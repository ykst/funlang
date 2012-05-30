fun f ()
    $print("f OK\n")
\_
    
fun g()
    $print("g OK\n")
    f()
\_

fun h(x,y)
    $print(x < y)
    $print("\n")
\_

g()
h(1,2)
h(2,1)
