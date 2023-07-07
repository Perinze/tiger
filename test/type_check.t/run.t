test1
  $ dune exec --no-build tiger type testcases/test1.tig

test2
  $ dune exec --no-build tiger type testcases/test2.tig

test3
  $ dune exec --no-build tiger type testcases/test3.tig

test4
  $ dune exec --no-build tiger type testcases/test4.tig

test5
  $ dune exec --no-build tiger type testcases/test5.tig

test6
  $ dune exec --no-build tiger type testcases/test6.tig

test7
  $ dune exec --no-build tiger type testcases/test7.tig

test8
  $ dune exec --no-build tiger type testcases/test8.tig

test9
  $ dune exec --no-build tiger type testcases/test9.tig
  testcases/test9.tig
  :3.0
  :error : types of then - else differ

test10
  $ dune exec --no-build tiger type testcases/test10.tig
  testcases/test10.tig
  :2.0
  :error : body of while not unit

test11
  $ dune exec --no-build tiger type testcases/test11.tig
  testcases/test11.tig
  :2.0
  :error : for-loop range must has type int

test12
  $ dune exec --no-build tiger type testcases/test12.tig

test13
  $ dune exec --no-build tiger type testcases/test13.tig
  testcases/test13.tig
  :3.2
  :error : integer required

test14
  $ dune exec --no-build tiger type testcases/test14.tig
  testcases/test14.tig
  :12.8
  :error : comparing equality between different types

test15
  $ dune exec --no-build tiger type testcases/test15.tig
  testcases/test15.tig
  :3.0
  :error : if-then returns non unit

test16
  $ dune exec --no-build tiger type testcases/test16.tig
  testcases/test16.tig
  :4.0
  :error : mutually recursive types that do not pass through record or array
  testcases/test16.tig
  :5.0
  :error : mutually recursive types that do not pass through record or array
  testcases/test16.tig
  :6.0
  :error : mutually recursive types that do not pass through record or array
  testcases/test16.tig
  :7.0
  :error : mutually recursive types that do not pass through record or array

test17
  $ dune exec --no-build tiger type testcases/test17.tig
  testcases/test17.tig
  :4.30
  :error : unknown type treelist

test18
  $ dune exec --no-build tiger type testcases/test18.tig
  testcases/test18.tig
  :5.14
  :error : unbound variable name do_nothing2
  testcases/test18.tig
  :5.14
  :error : not a function: do_nothing2
  testcases/test18.tig
  :5.14
  :error : formals are fewer than actuals

test19
  $ dune exec --no-build tiger type testcases/test19.tig
  testcases/test19.tig
  :8.15
  :error : undeclared variable a
  testcases/test19.tig
  :8.14
  :error : formals and actuals have different types

test20
  $ dune exec --no-build tiger type testcases/test20.tig
  testcases/test20.tig
  :3.17
  :error : undeclared variable i
  testcases/test20.tig
  :3.18
  :error : integer required

test21
  $ dune exec --no-build tiger type testcases/test21.tig
  testcases/test21.tig
  :8.10
  :error : integer required
  testcases/test21.tig
  :5.0
  :error : procedure returns value

test22
  $ dune exec --no-build tiger type testcases/test22.tig
  testcases/test22.tig
  :7.6
  :error : field nam not in record type
  testcases/test22.tig
  :7.10
  :error : type mismatch

test23
  $ dune exec --no-build tiger type testcases/test23.tig
  testcases/test23.tig
  :7.11
  :error : type mismatch
  testcases/test23.tig
  :8.9
  :error : type mismatch

test24
  $ dune exec --no-build tiger type testcases/test24.tig
  testcases/test24.tig
  :5.2
  :error : variable not array

test25
  $ dune exec --no-build tiger type testcases/test25.tig
  testcases/test25.tig
  :5.3
  :error : variable is not a record, cannot access its field
  testcases/test25.tig
  :5.3
  :error : field f not in record type

test26
  $ dune exec --no-build tiger type testcases/test26.tig
  testcases/test26.tig
  :3.2
  :error : integer required

test27
  $ dune exec --no-build tiger type testcases/test27.tig

test28
  $ dune exec --no-build tiger type testcases/test28.tig
  testcases/test28.tig
  :7.1
  :error : type constraint and init value differ

test29
  $ dune exec --no-build tiger type testcases/test29.tig
  testcases/test29.tig
  :7.1
  :error : type constraint and init value differ

test30
  $ dune exec --no-build tiger type testcases/test30.tig

test31
  $ dune exec --no-build tiger type testcases/test31.tig
  testcases/test31.tig
  :3.1
  :error : type constraint and init value differ

test32
  $ dune exec --no-build tiger type testcases/test32.tig
  testcases/test32.tig
  :6.18
  :error : initializing exp and array type differ

test33
  $ dune exec --no-build tiger type testcases/test33.tig
  testcases/test33.tig
  :3.17
  :error : unknown type rectype
  testcases/test33.tig
  :3.17
  :error : type rectype is not a record
  testcases/test33.tig
  :3.17
  :error : unknown type rectype

test34
  $ dune exec --no-build tiger type testcases/test34.tig
  testcases/test34.tig
  :5.2
  :error : formals and actuals have different types

test35
  $ dune exec --no-build tiger type testcases/test35.tig
  testcases/test35.tig
  :5.2
  :error : formals are more than actuals

test36
  $ dune exec --no-build tiger type testcases/test36.tig
  testcases/test36.tig
  :5.2
  :error : formals are fewer than actuals

test37
  $ dune exec --no-build tiger type testcases/test37.tig

test38
  $ dune exec --no-build tiger type testcases/test38.tig
  testcases/test38.tig
  :4.1
  :error : types with the same name in the same batch of mutually recursive types

test39
  $ dune exec --no-build tiger type testcases/test39.tig
  testcases/test39.tig
  :4.1
  :error : functions with the same name in the same batch of mutually recursive functions

test40
  $ dune exec --no-build tiger type testcases/test40.tig
  testcases/test40.tig
  :3.1
  :error : procedure returns value

test41
  $ dune exec --no-build tiger type testcases/test41.tig

test42
  $ dune exec --no-build tiger type testcases/test42.tig

test43
  $ dune exec --no-build tiger type testcases/test43.tig
  testcases/test43.tig
  :6.3
  :error : integer required

test44
  $ dune exec --no-build tiger type testcases/test44.tig

test45
  $ dune exec --no-build tiger type testcases/test45.tig
  testcases/test45.tig
  :5.1
  :error : initializing nil expressions not constrained by record type

test46
  $ dune exec --no-build tiger type testcases/test46.tig

test47
  $ dune exec --no-build tiger type testcases/test47.tig

test48
  $ dune exec --no-build tiger type testcases/test48.tig

test49
  $ dune exec --no-build tiger type testcases/test49.tig
  testcases/test49.tig
  :5.9
  :syntax error : nil should not be preceded by type-id

merge.tig
  $ dune exec --no-build tiger type merge.tig

queens.tig
  $ dune exec --no-build tiger type queens.tig
