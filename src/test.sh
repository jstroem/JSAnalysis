#!/bin/bash
scala JSAnalyzer.Test test.js > ast.dot
dot ast.dot -Tpng > ast.png
