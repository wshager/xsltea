xquery version "3.1";

import module namespace xsltea="http://lagua.nl/xquery/xsltea";

let $x := element root { element a { "test" }}
let $c := xsltea:create-context($x)
let $c := xsltea:template($c,"a",function($c,$n){
    element x { $n/string() }
})

return xsltea:apply-templates($c)
