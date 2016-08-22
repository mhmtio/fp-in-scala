package io.terrafino.fp

object sandbox {
  import List._
  val l = List(1, 2, 3, 4)                        //> l  : io.terrafino.fp.List[Int] = Cons(1,Cons(2,Cons(3,Cons(4,Nil))))
  val l2 = List(5, 6, 7)                          //> l2  : io.terrafino.fp.List[Int] = Cons(5,Cons(6,Cons(7,Nil)))
  length(l)                                       //> res0: Int = 4
  length(Nil)                                     //> res1: Int = 0
  length2(l)                                      //> res2: Int = 4
  length2(Nil)                                    //> res3: Int = 0
  append(l, l2)                                   //> res4: io.terrafino.fp.List[Int] = Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Cons(6,
                                                  //| Cons(7,Nil)))))))
  flatten(List(l, l2))                            //> res5: io.terrafino.fp.List[Int] = Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Cons(6,
                                                  //| Cons(7,Nil)))))))
  flatten2(List(l, l2))                           //> res6: io.terrafino.fp.List[Int] = Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Cons(6,
                                                  //| Cons(7,Nil)))))))
  
  filter(l)(x => x % 2 == 0)                      //> res7: io.terrafino.fp.List[Int] = Cons(2,Cons(4,Nil))
  flatMap(l)(x => List(x, x))                     //> res8: io.terrafino.fp.List[Int] = Cons(1,Cons(1,Cons(2,Cons(2,Cons(3,Cons(3,
                                                  //| Cons(4,Cons(4,Nil))))))))
  
  zipWith(l, l2)(_ + _)                           //> res9: io.terrafino.fp.List[Int] = Cons(6,Cons(8,Cons(10,Nil)))
  
  take(l, 2)                                      //> res10: io.terrafino.fp.List[Int] = Cons(1,Cons(2,Nil))
  
  hasSubSequence(List(), List(4, 3))              //> res11: Boolean = false
}