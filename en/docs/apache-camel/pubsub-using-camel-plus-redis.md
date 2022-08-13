---
title: "PubSub using Camel + Redis"
slug: "pubsub-using-camel-+-redis"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

**Using the publisher:**

    producerTemplate.asyncSendBody("direct:myprocedure", massageBody);

Using the "createProducer()" in ManagedCamel to create the producerTemplate.

## RedisPublisher
    public class RedisPublisher extends RouteBuilder {
    
        public static final String CAMEL_REDIS_CHANNEL = "CamelRedis.Channel";
        public static final String CAMEL_REDIS_MESSAGE = "CamelRedis.Message";
    
        @Value("${redis.host}")
        private String redisHost;
        @Value("${redis.port}")
        private int redisPort;
        @Value("${redis.channel.mychannel}")
        private String redisChannel;
    
        private String producerName;
    
        @Required
        public void setProducerName(String producerName) {
            this.producerName = producerName;
        }
    
        @Override
        public void configure() throws Exception {
            from(producerName)
                    .log(String.format("Publishing with redis in channel: %s, massage body: ${body}", redisChannel))
                    .setHeader(CAMEL_REDIS_CHANNEL, constant(redisChannel))
                    .setHeader(CAMEL_REDIS_MESSAGE, body())
                    .to(String.format("spring-redis://%s:%s?command=PUBLISH&redisTemplate=#%s", redisHost, redisPort, ManagedCamel.REDIS_TEMPLATE));
        }
    }

## RedisSubscriber
    public class RedisSubscriber extends RouteBuilder {
    
        @Value("${redis.host}")
        private String redisHost;
        @Value("${redis.port}")
        private int redisPort;
        @Value("${redis.channel.mychannel}")
        private String redisChannel;
    
        private Object bean;
        private String method;
    
        @Required
        public void setBean(Object bean) {
            this.bean = bean;
        }
    
        @Required
        public void setMethod(String method) {
            this.method = method;
        }
    
        @Override
        public void configure() throws Exception {
            from(String.format("spring-redis://%s:%s?command=SUBSCRIBE&channels=%s&serializer=#%s", redisHost, redisPort, redisChannel, ManagedCamel.REDIS_SERIALIZER))
                    .log(String.format("Consuming with redis in channel: %s, massage body: ${body}", redisChannel))
                    .process(exchange -> {
                    }).bean(bean, String.format("%s(${body})", method));
        }
    }

The Method 'method' inside the injected bean will handle the massages recived.


## Subscriber spring context
    <bean id="managedCamel" class="com.pubsub.example.ManagedCamel" >
        <constructor-arg name="routes">
            <list>
                <ref bean="redisSubscriber"/>
            </list>
        </constructor-arg>
    </bean>

    <bean id="redisSubscriber" class="com.pubSub.example.RedisSubscriber" >
        <property name="bean" ref="myBean"/>
        <property name="method" value="process"/>
    </bean>

## Publisher spring context
    <bean id="managedCamel" class="com.pubSub.example.ManagedCamel" >
        <constructor-arg name="routes">
            <list>
                <ref bean="redisPublisher"/>
            </list>
        </constructor-arg>
    </bean>

    <bean id="redisPublisher" class="com.pubSub.example.RedisPublisher" >
        <property name="producerName" value="direct:myprocedure"/>
    </bean>

## ManagedCamel
    public class ManagedCamel implements Managed {
    
        public static final String REDIS_TEMPLATE = "redisTemplate";
        public static final String LISTENER_CONTAINER = "listenerContainer";
        public static final String REDIS_SERIALIZER = "redisSerializer";
        private DefaultCamelContext camelContext;
    
        private List<RouteBuilder> routes;
        @Value("${redis.host}")
        private String redisHost;
        @Value("${redis.port}")
        private int redisPort;
        @Value("${redis.password}")
        private String redisPassword;
    
        public ManagedCamel(List<RouteBuilder> routes) throws Exception {
            this.routes = routes;
        }
    
        @PostConstruct
        private void postInit() throws Exception {
            JndiRegistry registry = new JndiRegistry();
            final StringRedisSerializer serializer = new StringRedisSerializer();
            RedisTemplate<String, Object> redisTemplate = getRedisTemplate(serializer);
            registry.bind(REDIS_TEMPLATE, redisTemplate);
            RedisMessageListenerContainer messageListenerContainer = new RedisMessageListenerContainer();
            registry.bind(LISTENER_CONTAINER, messageListenerContainer);
            registry.bind(REDIS_SERIALIZER, serializer);
    
            camelContext = new DefaultCamelContext(registry);
            for (RouteBuilder routeBuilder : routes) {
                camelContext.addRoutes(routeBuilder);
            }
            start();
        }
    
        private RedisTemplate<String, Object> getRedisTemplate(StringRedisSerializer serializer) {
            RedisTemplate<String, Object> redisTemplate = new RedisTemplate<String, Object>();
            redisTemplate.setConnectionFactory(redisConnectionFactory());
            redisTemplate.setKeySerializer(new StringRedisSerializer());
            redisTemplate.setValueSerializer(serializer);
            redisTemplate.setEnableDefaultSerializer(false);
            redisTemplate.afterPropertiesSet();
            return redisTemplate;
        }
    
        private RedisConnectionFactory redisConnectionFactory() {
            final JedisConnectionFactory jedisConnectionFactory = new JedisConnectionFactory();
            jedisConnectionFactory.setHostName(redisHost);
            jedisConnectionFactory.setPort(redisPort);
            jedisConnectionFactory.setPassword(redisPassword);
            jedisConnectionFactory.afterPropertiesSet();
            return jedisConnectionFactory;
        }
    
        public void start() throws Exception {
            camelContext.start();
        }
    
        public void stop() throws Exception {
            camelContext.stop();
        }
    
        public ProducerTemplate createProducer() {
            return camelContext.createProducerTemplate();
        }
    
    }



