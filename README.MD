本次分享内容：

1、Monadic programming style :   
      Reader Monad and Monad Transformer 
      for Dependancy Injection 
      and DataAccess Result Types 
      - @雪川大虫

2、一种简单可靠的基于Akka的分布式缓存系统设计和实现
      - @凤凰木

3、泛型派生技术内幕 - Circe实践： 
    insight "Generic derivation" by Travis Brown
      - @水山清风
      
分享主题小结：

  @雪川大虫分享了Monix.Task替代Future用法，以及如何通过ReaderMonad
和MonadTransformer来构建数据库操作函数的运算结果类型：
1、用ReaderMonad组成的函数结果类型来实现依赖注入。函数返回结果使用这种类型
   能使用户在软件开发和测试过程中很方便的把不同类型的数据库、运行环境配置等注入函数
2、通过MonadTransformer实现对Task[Either[String,Option[R]]]这种数据库
   数据处理可能经常遇到的类型使用。
  
  @凤凰木分享了在akka-cluster环境内protobuf消息发布/订阅的实现方式。
  示范虽然是针对一个特定项目，但仍然有许多地方可以借鉴：
1、akka-cluster的配置示范：对我来说，其中的配置参数可能需要时间和精力去尝试
2、如何让akka-cluster在发布/订阅操作中使用protobuf消息（我记得akka的系统消息序列化
   用的都是java object，gossip消息交换除外，那是用protobuf的）。这里面包括了如何
   配置akka的serializer和实现protobuf消息serializer和具体序列化  
3、akka自身的一些监管策略包括：SuppervisionStrategy,backOffStrategy、
   SourceRestart的编程示范 

  @水山清风分享了circe各种版本encoder,decoder的推断原理和方式，以及该等encode,
  decoder在进行cass class与json之间进行类型转换的方式和原理，与之相应的转换效率分析。
  可惜，并没有circe这个工具的具体使用和编程示范。这对我这样的circe门外汉来说，
  好像有跳跃的感觉。anyhow, 对circe的编译和json解析原理还是有了些印象，
  算是一种收获吧          