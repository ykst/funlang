fun factorial(n)

    if n <= 1
        return 1
    \_

    return n * factorial(n - 1)
\___

fun factorialT(n)

    fun sub(n, ret)
        if n <= 1
            return ret
        \_

        return sub(n - 1, ret * n)
    \__


    return sub(n, 1)
\___

$print(factorial(100))
$print("\n")

$print(factorialT(100))
$print("\n")

$print(factorial(0))
$print("\n")

$print(factorialT(0))
$print("\n")
