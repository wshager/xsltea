xquery version "3.1";

import module namespace xsltea="http://lagua.nl/xquery/xsltea" at "xsltea.xqm";

let $x := element root { element a { "test" }}
let $c := xsltea:create-context($x)
let $c := xsltea:template($c,"root",function($c,$n){
    element x {
        xsltea:apply-templates($c)
    }
})
let $c := xsltea:template($c,"a",function($c,$n){
    element y {
        xsltea:apply-templates($c)
    }
})

return xsltea:apply-templates($c)
