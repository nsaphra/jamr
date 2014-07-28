package edu.cmu.lti.nlp.amr.JointDecoder
import edu.cmu.lti.nlp.amr._

import scala.reflect.ClassTag
import scala.util.Sorting.stableSort

class SortedDoubleLinkedList[A: ClassTag](list: Seq[A],
    sort_fun: (A, A) => Boolean) extends DoubleLinkedList[A](stableSort(list, sort_fun)) {

    def insertSomewhereAfter(data: A, after: Option[LinkedNode[A]]) : LinkedNode[A] = {
        var cur: Option[LinkedNode[A]] = after
        while (cur.isDefined) {
            if (cur.get.next.isDefined &&
                sort_fun(cur.get.next.get.data, data)) {
                cur = cur.get.next
            } else {
                return this.insert(data, cur)
            }
        }
        return this.insert(data, cur)
    }

    def insertAll(new_data: Seq[A]) : Array[LinkedNode[A]] = {
        val sorted_data = stableSort(new_data, sort_fun)
        var cur_after: Option[LinkedNode[A]] = top
        for (datum <- sorted_data) yield {
            cur_after = Some(insertSomewhereAfter(datum, cur_after))
            cur_after.get
        }
    }
}
