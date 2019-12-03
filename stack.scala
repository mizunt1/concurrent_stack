3//Modify the unbounded lock-free stack from lectures to work in the absence
//of a garbage collector. 
//Give each thread a pool of previously used nodes, and
//allow nodes to be recycled. You will need to think carefully about how to
//avoid the ABA problem.


class LockFreeStack[T] extends TotalStack[T]{
  private class Node(val value:T){
    var next: Node = null
    val nextStamp = new AtomicPair[Node,Int](null, 0)
    def next = nextStamp.getFirst
    def stamp = nextStamp.getSecond
  }
  private val freeList = Array.fill(p)(null: Node)
  private val firstNode = new Node(null)
  private val top = new AtomicPair[Node,Int](firstNode, 0)
  private def pause = ox.cads.util.Spin(500)

  def allocate(value: T): Node = {
    val me = ThreadID.get
    if(freeList(me) == null) new Node(value)
    else{
      val n = freeList(me);
      freeList(me) = freeList(me).next.get
      n.value = value;
      n.next.set(null);n
    }
  }
  def free(n: Node) = {
    val me = ThreadID.get; n.next.set(freeList(me)); freeList(me) = n
  }

  
  def push(value: T) = {
    val node = new allocate(value)
    var done = false
    do {
      val (oldTop, tStamp) = top.get
      // pointer to top node
      val (next, nStamp) = node.nextStamp.get
      // the next node after the top node
      // should be null at this point
      //get the stamp from oldTop. Every time oldTop gets recyled,
      if((oldTop, tStamp) == top.get){
        // pointers to top still unchanged at this point
        if (next == null){
          if(node.nextStamp.compareAndSet((next, nStamp), (oldTop, nStamp+1))){
            // change next pointer of node from null to oldTop
            top.compareAndSet((oldTop, tStamp),(node, tStamp+1));
            // change the pointer to top from oldTop to node
            done = true
         }
        }
      // next has been changed, maybe dont need
      else
        top.compareAndSet((oldTop, tStamp), (next, tStamp+1))
      // move pointer along, next is already set
      }
      if(!done) pause
    } while(!done)
      }

  def pop : Option[T] = {
    var result : Option[T] = None; var done = false
    do{
      val (oldTop, tStamp) = top.get
      val (next, nStamp) = oldTop.nextStamp.get
      if(oldTop == null) done = true // empty stack; return None
      else{
        val (newTop, oStamp) = oldTop.nextStamp.get
        // try to remove oldTop from list
        // not sure about below
        if(top.compareAndSet((oldTop, tStamp), (newTop,tStamp+1))){
          // change top from oldTop to newTop. newTop defined as oldTop.next
          // since we are popping
          result = Some(oldTop.value); free(oldTop); done = true
          
        }
        else pause
      }
    } while(!done)
      result
  }

// Dont know how to right a linearlisability tester
def main() = {
 /** types for the LinTesters */
  var reps = 1250
  var p = 4
 type SeqType = scala.collection.immutable.List[Int]
 type ConcQueue = ox.cads.TotalStack
  /** sequential behaviour we expect */
  def seqPush(x: Int)(s: SeqType) : (Unit, SeqType) =
    ((), x :: s)
  def seqPop(s: SeqType) : (Option[Int], SeqType) =
    s match {
      case Nil => (None, s)
      case x :: newS => (Some(x), newS)
    }
  def worker

}
