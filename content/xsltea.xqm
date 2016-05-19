(: XSLTea: An XSLT implementation in Xquery :)
(: Author: Wouter Hager :)
(: Copyright: Lagua 2016 :)

xquery version "3.1";

module namespace xsltea="http://lagua.nl/xquery/xsltea";

(:import module namespace console="http://exist-db.org/xquery/console";:)

declare function xsltea:template($c,$q,$fn) {
    xsltea:template($c,$q,$fn,1)
};
declare function xsltea:template($c,$q,$fn,$prio){
    xsltea:template($c,$q,$fn,$prio,())
};

declare function xsltea:template($c,$q,$fn,$prio,$mode){
    map:put($c,"templates",xsltea:insert-template($c("templates"),$q,$fn,$prio,$mode,false()))
};

declare function xsltea:insert-template($templates,$q,$fn,$prio,$mode,$default){
    (: check if there's a union :)
    if(matches($q,"\|")) then
        fold-left(tokenize($q,"\|"),$templates,function($pre,$cur){
            xsltea:insert-template($pre,replace($cur,"^\s+|\s+$",""),$fn,$prio,$mode,$default)
        })
    else
    (: do we have a rule for this query ? :)
    if(map:contains($templates,$q)) then
        let $rules := $templates($q)
        (: get the lowest prio rule, which should be the first :)
        let $r := array:head($rules)
        (: do we have this prio and is it not built-in? :)
        return
            if($r("prio") eq $prio and $r("default") eq false()) then
                (: we already have this prio, so increment it :)
                xsltea:insert-template($templates,$q,$fn,$prio + 1,$mode,$default)
            else
                map:put($templates,$q,array:append($rules,map {
                    "q" := $q,
                    "prio" := $prio,
                    "weight" := xsltea:weigh-rule($q,$prio),
                    "fn" := $fn,
                    "default" := $default
                }))
    else
        map:put($templates,$q,[
            map {
                "q" := $q,
                "prio" := $prio,
                "weight" := xsltea:weigh-rule($q,$prio),
                "fn" := $fn,
                "default" := $default
            }
        ])
};

declare function xsltea:weigh-rule($q,$prio) {
    let $parts := tokenize($q,"\[")
    let $prio := if(count($parts) > 1) then $prio - .5 else $prio
    let $q := $parts[1]
    let $prio := if(matches($q,"(^|[^:])\*")) then $prio + .5 else $prio
    let $prio := if(matches($q,":\*")) then $prio + .25 else $prio
    let $prio := if(matches($q,"^(node|element|attribute|text|comment|processing-instruction)\(\)")) then $prio + .5 else $prio
    let $prio := if(matches($q,"/")) then $prio - .25 else $prio
    return $prio
};

declare function xsltea:create-context($root){
    let $templates := map {}
    let $templates := xsltea:insert-template($templates,"/|*",function($c,$n){
        xsltea:apply-templates($c,$n/node())
    },1,(),true())
    return
        map {
            "root" := $root,
            "templates" := $templates
        }
};

declare function xsltea:get-ancestor-match($anc,$node){
    let $match := name($node) eq head($anc)
    return
        if($match and count($anc)>1) then
            xsltea:get-ancestor-match(tail($anc),$node/..)
        else
            $match
};

declare function xsltea:get-match($q,$node){
    if(matches($q,"^\*")) then
        $node instance of node()
    else if(matches($q,"^@\*")) then
        $node instance of attribute()
    else if(matches($q,"^(node|element|attribute|text|comment|processing-instruction)\(")) then
        util:eval("$n instance of " || $q)
    else if(matches($q,"/")) then
        let $parts := reverse(tokenize($q,"/"))
        return
            if(xsltea:get-ancestor-match(tail($parts),$node/..)) then
                xsltea:get-match(head($parts),$node)
            else
                false()
    else if(matches($q,"\[")) then
        let $parts := tokenize($q,"\[")
        return
            if(xsltea:get-match(head($parts),$node)) then
                let $ret := util:eval("$node[" || string-join(tail($parts),"["))
                return exists($ret)
            else
                false()
    else if($node instance of element() or $node instance of attribute()) then
        name($node) eq $q
    else
        $node eq $q
};

declare function xsltea:apply-templates($context){
    let $context := 
        if(map:contains($context,"current")) then
            $context
        else
            map:put($context,"current",$context("root"))
    return xsltea:apply-templates($context,$context("current"))
};

declare function xsltea:apply-templates($context,$node) {
    let $rules := fold-left(map:keys($context("templates")),[],function($pre,$q){
        let $match := 
            if(matches($q,"^/")) then
                (: $node should match root :)
                if($node eq $context("root")) then
                    if(matches($q,"^/$")) then
                        true()
                    else
                        xsltea:get-match(replace($q,"^/",""),$node)
                else
                    false()
            else
                xsltea:get-match($q,$node)
        return 
            if($match) then
                (: insert the rules :)
                array:fold-left($context("templates")($q),$pre,function($pre,$_){
                    array:append($pre,$_)
                })
            else
                $pre
    })
    (: weigh the rules (using ancient sort) :)
    let $weights := for $w at $i in array:flatten(array:for-each($rules,function($_) { $_("weight") }))
        order by $w
        return $i
    let $chosen := fold-left($rules?($weights),(),function($pre,$cur){
        if($pre instance of map(xs:string,item()?)) then
            if($pre("default") and $cur("default") = false()) then
                $cur
            else
                $pre
        else
            $cur
    })
    return $chosen("fn")($context,$node)
};