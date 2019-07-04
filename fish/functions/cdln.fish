# Change to the directory a symbolically linked file point to.
function cdln
    cd (dirname (readlink $argv))
end
