fun f(x)

    var priv = x
    
    fun getPriv()
        return priv
    \_
\_
    
var a = f{"A"}
var b = f{"B"}

$print(a.getPriv() ++ "\n")
$print(b.getPriv() ++ "\n")
$print(b.x ++ "\n")
$print(a.x ++ "\n")
