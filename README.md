XSLTea
===

XSLT implementation written in Xquery

To install in eXist-db:
--------------------

Download and install eXist-db 3 at http://exist-db.org

Build the package and install into eXist using the manager in the dashboard.

--------

XSLTea implements common XSLT functions in Xquery for your convenience. It's inspired by the great work of Michael Kay, the king of XSLT and fervent admirer of tea.

Since XSLT presupposes a context, you have to pass it around yourself in Xquery. To create a context, simply call the function `xsltea:create-context($root-node)`.

Now you can start adding templates to the context by calling `xsltea:template($context,$match,$function,$priority,$mode)`, where the provided function will be executed with the context and the current match when it has been selected by the processor. The run the processor, call `xsltea:apply-templates($context)`.
