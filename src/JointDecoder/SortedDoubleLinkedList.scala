package edu.cmu.lti.nlp.amr.JointDecoder
import edu.cmu.lti.nlp.amr._

import scala.collection.mutable.List

class SortedDLL(list: List[A],
                key_fun: A => math.Ordering) extends DoubleLinkedList[A](stableSort(list, key_fun)) {

    def insertSomewhereAfter(e: Fragment,
                             var after: Option[Link]) : LinkedNode[A] = {
        var cur: Option[Link] = after
        while (cur != None) {
            after = cur
            link = cur match { case Some(x) => x }
            cur = link.next match {
                case None => None
                case Some(next) => {
                    if (key_fun(next.data) > key_fun(link.data)) {
                        None
                    } else {
                        x.next
                    }
                }
            }
        }
        return queue.insert(e, after)
    }

    def insertAll(new_data: List[A]) : List[LinkedNode[A]] = {
        sorted_data = stableSort(new_data, (x,y) => key_fun(x) < key_fun(y))
        var cur_after: Option[Link] = head
        for (datum <- sorted_data) {
            new_elt = insertSomewhereAfter(elt.data, cur_after)
            cur_after = Some(new_elt)
            yield new_elt
        }
    }
}
