#####
# basic checks
if true
    $print("OK\n")
\_

if false
    $print("NG\n")
else
    $print("OK\n")
\_

if false
    $print("NG\n")
elsif true 
    $print("OK\n")
else
    $print("NG\n")
\_

#####
# empty block
if true
\_

if false
else
\_

if false
elsif true 
else
\_
