fun iterate(obj)
    while obj.cond()
        obj.proc()
        obj.post()
    \__
\__

fun counter()
    var i = 0

    fun countUp()
        i = i + 1
        return i
    \_

    return countUp
\_

fun foobar(num)
    var i = 0
    var c = counter()

    fun cond()
        
        return i <= num
    \__

    fun proc()
        if i % 15 == 0
            $print("FooBar\n")
        elsif i % 3 == 0
            $print("Foo\n")
        elsif i % 5 == 0
            $print("Bar\n")
        else
            $print(i)
            $print("\n")
        \__
    \__

    fun post()
        i = c()
    \_
\__

$eval("iterate" ++ "(foobar{30})")
