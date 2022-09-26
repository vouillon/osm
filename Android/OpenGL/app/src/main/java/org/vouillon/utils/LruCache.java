package org.vouillon.utils;
import java.util.HashMap;

public class LruCache {

	private int size, maxSize;
	BaseNode lst;
	
	public LruCache (int sz) {
		maxSize = sz;
		size = 0;
	}

	private static abstract class BaseNode {
		BaseNode (LruCache cache) {
			BaseNode n = (cache.lst != null)?cache.lst:this;
			prev = next = n;
			if (cache.lst != null) insertBefore (cache.lst);
			cache.lst = this;
		}
		abstract void remove ();
		BaseNode prev, next;
		void removeFromList () {
			prev.next = next;
			next.prev = prev;
		}
		void insertBefore (BaseNode n) {
			prev = n.prev;
			next = n;
			n.prev.next = this;
			n.prev = this;
		}
		void moveToFront (LruCache cache) {
			BaseNode n = cache.lst;
			if (n != this) {
				removeFromList();
			    insertBefore(n);
			    cache.lst = this;
			}
		}
	}

	public static abstract class Cache<K, T> {
		private HashMap<K, Node> hashMap;
		private LruCache cache;

		class Node extends BaseNode {
			K key;
			T elt;
			Node(K k, T e) {
				super(cache);
				key = k; elt = e;
			}
			void remove () {
				hashMap.remove(key);
			}
		}
		
		public Cache (LruCache c) {
			cache = c;
			hashMap = new HashMap<>();
		}

		protected abstract T get(K i);

		public T find (K i) {
			Node n = hashMap.get(i);
			if (n != null) {
//				System.out.println("HIT");
				n.moveToFront(cache);
				return n.elt;
			} else {
//				System.out.println("MISS");
				T e = get(i);
 //               Log.d("LruCache", "size: " + cache.size);
				if (cache.size == cache.maxSize) {
					BaseNode n2 = cache.lst.prev;
					n2.removeFromList();
					n2.remove();
				} else
					cache.size++;
				n = new Node(i, e);
				hashMap.put(i, n);
				return e;
			}
		}
		
	}
}
