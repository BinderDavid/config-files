# Open a ghci session which spits out Core versions of expressions.
function ghci-core
    ghci -ddump-simpl -dsuppress-idinfo \
    -dsuppress-coercions -dsuppress-type-applications \
    -dsuppress-uniques -dsuppress-module-prefixes
end
