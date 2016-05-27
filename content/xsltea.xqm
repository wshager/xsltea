(: XSLTea: An XSLT implementation in Xquery :)
(: Author: Wouter Hager :)
(: Copyright: Lagua 2016 :)

xquery version "3.1";

module namespace xsltea="http://lagua.nl/xquery/xsltea";
declare namespace xsl="http://www.w3.org/1999/XSL/Transform";
declare namespace a="http://lagua.nl/xquery/array-util";
declare namespace s="http://lagua.nl/xquery/seq-util";

declare function a:fold-left($array as array(item()?),$zero,$function){
	if(array:size($array) eq 0) then
		$zero
	else
		a:fold-left(array:tail($array), $function($zero, array:head($array)), $function )
};


declare function s:fold-left($seq as item()*,$zero,$function){
	if(count($seq) eq 0) then
		$zero
	else
		s:fold-left(tail($seq), $function($zero, head($seq)), $function )
};

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
        s:fold-left(tokenize($q,"\|"),$templates,function($pre,$cur){
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
    let $prio := if(count($parts) > 1) then $prio + .5 else $prio
    let $q := $parts[1]
    let $prio := if(matches($q,"(^|[^:])\*[^:]")) then $prio - .5 else $prio
    let $prio := if(matches($q,":\*")) then $prio - .25 else $prio
    let $prio := if(matches($q,"^(node|element|attribute|text|comment|processing-instruction)\(\)")) then $prio - .5 else $prio
    let $prio := if(matches($q,"/")) then $prio + .5 else $prio
    return $prio
};

declare function xsltea:value-of($c,$node){
    $node/data()
};

declare function xsltea:create-context($root){
    xsltea:create-context($root,map {})
};

declare function xsltea:create-context($root,$params){
    let $templates := map {}
    let $templates := xsltea:insert-template($templates,"/|*",function($c,$n){
        xsltea:apply-templates($c,$n/node())
    },1,(),true())
    let $templates := xsltea:insert-template($templates,"text()|@*",function($c,$n){
        $n/data()
    },1,(),true())
    return
        map {
            "root" := $root,
            "frame": $params,
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
        $node instance of element()
    else if(matches($q,"^@\*")) then
        $node instance of attribute()
    else if(matches($q,"^(node|element|attribute|text|comment|processing-instruction)\(")) then
        util:eval("$node instance of " || $q)
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
    let $node :=
        if(map:contains($context,"current")) then
            $context("current")/node()
        else
            $context("root")
    return xsltea:apply-templates($context,$node)
};

declare function xsltea:collect-rules($templates,$node){
    fold-left(map:keys($templates),[],function($pre,$q){
        let $match :=
            if(matches($q,"^/")) then
                (: $node should match root :)
                if($node instance of element() and empty($node/..)) then
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
                a:fold-left($templates($q),$pre,function($pre,$_){
                    array:append($pre,$_)
                })
            else
                $pre
    })
};

declare function xsltea:apply-templates($context,$nodes) {
    for-each($nodes,function($node){
        let $context := map:put($context,"current",$node)
        let $rules := array:flatten(xsltea:collect-rules($context("templates"),$node))
        return if(count($rules) > 0) then
            (: weigh the rules (using ancient sort) :)
            let $ordered :=  for $_ in $rules
                order by $_("default"), $_("weight") descending
                return $_
            return $ordered[1]("fn")($context,$node)
        else
            ()
    })
};

declare function xsltea:transform($root,$xsl) {
    xsltea:transform($root,$xsl,map {})
};

declare function xsltea:transform($root,$xsl,$params) {
    xsltea:apply-templates(xsltea:convert(xsltea:create-context($root,$params),$xsl))
};

declare function xsltea:convert($c,$xsl) {
    (: get all nodes from parent, iterate over them with folding function :)
    if($xsl/node()) then
        fold-left($xsl/node(),$c,function($pre,$cur){
            typeswitch($cur)
            case element() return
                if($cur/namespace-uri() eq "http://www.w3.org/1999/XSL/Transform") then
                    switch(local-name($cur))
                        case "template" return
                            xsltea:template($pre,$cur/@match/string(),function($c,$n) {
                                xsltea:apply(map:put($c,"result",()),$cur,$n)("result")
                            },if(exists($cur/@priority)) then
                                number($cur/@priority)
                            else
                                1,$cur/@mode/string())
                        default return $pre
                else
                    $pre
            default return $pre
        })
    else
        $c
};

declare function xsltea:append-child($p as element(),$c as item()) {
    element { name($p) } {
        $p/@*,
        $p/*,
        $c
    }
};

declare function xsltea:insert-attr-data($p as attribute(),$c as xs:anyAtomicType*){
    attribute { name ($p) } {
        $p/data(),
        $c
    }
};

declare function xsltea:insert-text-data($p as text(),$c as xs:anyAtomicType*){
    text {
        $p/data(),
        $c
    }
};


declare function xsltea:insert-result($c,$n){
    let $p := $c("result")
    let $ret :=
        typeswitch($p)
            case element() return xsltea:append-child($p,$n)
            case attribute() return xsltea:insert-attr-data($p,$n)
            case text() return xsltea:insert-text-data($p,$n)
            default return ($p,$n)
    return map:put($c,"result",$ret)
};

declare function xsltea:apply($c,$xsl,$node) {
    (: if the node is an xsl:template element, fold it into the context :)
    (: if the node is some other xsl element, call the specified function :)
    (: otherwise return the node :)
    if($xsl/node()) then
        s:fold-left($xsl/node(),$c,function($pre,$cur){
            typeswitch($cur)
            case element() return
                if($cur/namespace-uri() eq "http://www.w3.org/1999/XSL/Transform") then
                    switch(local-name($cur))
                        case "apply-templates" return
                            let $ret := xsltea:apply-templates($pre,if($cur/@match) then util:eval("$node/" || $cur/@match) else $node/node())
                            return xsltea:insert-result($pre,$ret)
                        case "value-of" return
                            let $val := s:fold-left(map:keys($pre("frame")),$cur/@select/string(),function($acc,$cur){
                                replace($acc,"\$" || $cur,concat("'",$pre("frame")($cur),"'"))
                            })
                            return xsltea:insert-result($pre, string(util:eval("$node/" || $val)))
                        case "copy-of" return
                            xsltea:insert-result($pre,util:eval("$node/" || $cur/@select))
                        case "copy" return
                            xsltea:insert-result($pre,$node)
                        case "variable" return
                            let $val := if($cur/@select) then util:eval("$node/" || $cur/@select/string()) else xsltea:apply($pre,$cur,$node)("result")
                            return
                                map:put($pre,"frame",map:put($pre("frame"),$cur/@name/string(),$val))
                        default return xsltea:insert-result($pre,xsltea:apply-templates($pre,if($cur/@match) then util:eval("$node/" || $cur/@match) else $node/node()))
                else
                    xsltea:insert-result($pre,element { name($cur) } {  xsltea:apply($pre,$cur,$node)("result") })
            case text() return xsltea:insert-result($pre,$cur)
            default return xsltea:insert-result($pre,$cur)
        })
    else
        $c
};
