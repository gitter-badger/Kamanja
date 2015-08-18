import org.junit._
import org.junit.Assert._
import com.ligadata.keyvaluestore._

/*
 * create 'default', 'value'
 *
 * put 'default', 'KEYKEY', 'value', 'ValueValue'
 *
 * scan 'default'
 *
 */

class BasicHBaseUnitTest
{
	//@Test
	def testHBaseLocal()
	{
		val connectinfo = new PropertyMap

		connectinfo+= ("connectiontype" -> "hbase")
		connectinfo+= ("hostlist" -> "localhost")
		connectinfo+= ("schema" -> "default")
		connectinfo+= ("table" -> "default")

		val store = KeyValueManager.Get(connectinfo)

		object i extends IStorage
		{
			val key = new Key
			val value = new Value
			key +=(1,2,3,4)
			value +=(10, 11, 12, 13, 14, 15, 16,17, 18, 19, 20)

			def Key = key
			def Value = value
			def Construct(Key: Key, Value: Value) = {}
		}

		object o extends IStorage
		{
			var key = new com.ligadata.keyvaluestore.Key;
			var value = new com.ligadata.keyvaluestore.Value

			def Key = key
			def Value = value
			def Construct(k: com.ligadata.keyvaluestore.Key, v: com.ligadata.keyvaluestore.Value) =
			{
				key = k;
				value = v;
			}
		}

		store.put(i)
		store.get(i.Key, o)
		store.del(i)

		assertEquals(i.Key, o.Key)
		assertEquals(i.Value, o.Value)

		store.Shutdown()

		println("testHBaseLocal - Done")
	}

	//@Test
	def testHBaseLocalAdd()
	{
		val connectinfo = new PropertyMap

		connectinfo+= ("connectiontype" -> "hbase")
		connectinfo+= ("hostlist" -> "localhost")
		connectinfo+= ("schema" -> "default")
		connectinfo+= ("table" -> "default")

		val store = KeyValueManager.Get(connectinfo)

		object i extends IStorage
		{
			val key = new Key
			val value = new Value
			key +=(1,2,3,5)
			value +=(10, 11, 12, 13, 14, 15, 16,17, 18, 19, 20)

			def Key = key
			def Value = value
			def Construct(Key: Key, Value: Value) = {}
		}

		object o extends IStorage
		{
			var key = new com.ligadata.keyvaluestore.Key;
			var value = new com.ligadata.keyvaluestore.Value

			def Key = key
			def Value = value
			def Construct(k: com.ligadata.keyvaluestore.Key, v: com.ligadata.keyvaluestore.Value) =
			{
				key = k;
				value = v;
			}
		}

		store.add(i)

		try
		{
			store.add(i) // Expect exception here
		}
		catch
		{
			case e: Exception => println("Caught exception " + e.getMessage() + "\n" /*+ e.getStackTraceString*/)
		}

		store.get(i.Key, o)
		store.del(i)

		assertEquals(i.Key, o.Key)
		assertEquals(i.Value, o.Value)

		store.Shutdown()

		println("testHBaseLocalAdd - Done")
	}

	//@Test
	def testGetAllKeysLocal()
	{
		val connectinfo = new PropertyMap

		connectinfo+= ("connectiontype" -> "hbase")
		connectinfo+= ("hostlist" -> "localhost")
		connectinfo+= ("schema" -> "default")
		connectinfo+= ("table" -> "default")

		val store = KeyValueManager.Get(connectinfo)

		store.TruncateStore()

		object i extends IStorage
		{
			val key = new Key
			val value = new Value
			key +=(0,2,3,4)
			value +=(10, 11, 12, 13, 14, 15, 16,17, 18, 19, 20)

			def Key = key
			def Value = value
			def Construct(Key: Key, Value: Value) = {}
		}


		object o extends IStorage
		{
			var key = new com.ligadata.keyvaluestore.Key;
			var value = new com.ligadata.keyvaluestore.Value

			def Key = key
			def Value = value
			def Construct(k: com.ligadata.keyvaluestore.Key, v: com.ligadata.keyvaluestore.Value) =
			{
				key = k;
				value = v;
			}
		}

		for( a <- 1 to 100)
		{
		  	i.key(0) = a.toByte
			store.put(i)
		}

		var keys = scala.collection.mutable.Set[String]()

		store.getAllKeys( {(key : Key) => keys.add(key.toString) } )

		assertEquals(100, keys.size)

/*
		for(key <- keys)
		{
			println(key)
		}
*/
		store.Shutdown()

		println("testGetAllKeysLocal - Done")
	}
}