package com.chiu.megalith.search.mq.handler;


import com.chiu.megalith.exhibit.entity.BlogEntity;
import com.chiu.megalith.exhibit.repository.BlogRepository;
import com.chiu.megalith.base.lang.Const;
import com.chiu.megalith.base.search.BlogIndexEnum;
import com.chiu.megalith.base.search.BlogSearchIndexMessage;
import com.rabbitmq.client.Channel;
import lombok.SneakyThrows;
import org.redisson.api.RLock;
import org.redisson.api.RedissonClient;
import org.springframework.amqp.core.Message;
import org.springframework.amqp.rabbit.connection.PublisherCallbackChannel;
import org.springframework.data.redis.core.StringRedisTemplate;
import java.time.LocalDateTime;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;


public abstract sealed class BlogIndexSupport permits
        CreateBlogIndexHandler,
        RemoveBlogIndexHandler,
        UpdateBlogIndexHandler {

    protected StringRedisTemplate redisTemplate;

    protected BlogRepository blogRepository;

    protected RedissonClient redisson;

    protected final RLock rLock;

    protected BlogIndexSupport(StringRedisTemplate redisTemplate,
                               BlogRepository blogRepository,
                               RedissonClient redisson) {
        this.redisTemplate = redisTemplate;
        this.blogRepository = blogRepository;
        this.redisson = redisson;
        rLock = redisson.getLock("redisBlogProcess");
    }

    public abstract boolean supports(BlogIndexEnum blogIndexEnum);
    protected abstract void redisProcess(BlogEntity blog);
    protected abstract void elasticSearchProcess(BlogEntity blog);

    @SneakyThrows
    public void handle(BlogSearchIndexMessage message,
                       Channel channel,
                       Message msg) {
        String createUUID = msg.getMessageProperties().getHeader(PublisherCallbackChannel.RETURNED_MESSAGE_CORRELATION_KEY);
        long deliveryTag = msg.getMessageProperties().getDeliveryTag();
        if (Boolean.TRUE.equals(redisTemplate.hasKey(Const.CONSUME_MONITOR.getInfo()  + createUUID))) {
            try {
                Long blogId = message.getBlogId();
                Integer year = message.getYear();
                BlogEntity blogEntity = blogRepository.findById(blogId).
                        orElseGet(() -> BlogEntity.
                                builder().
                                id(blogId).
                                created(LocalDateTime.of(year, 1,1,1 ,1 ,1, 1)).
                                build());

                if (!rLock.tryLock(5, TimeUnit.SECONDS)) {
                    throw new TimeoutException("get lock timeout");
                }

                try {
                    redisProcess(blogEntity);
                } finally {
                    rLock.unlock();
                }

                elasticSearchProcess(blogEntity);
                //??????????????????
                //false??????????????????????????????
                channel.basicAck(deliveryTag, false);
                redisTemplate.delete(Const.CONSUME_MONITOR.getInfo() + createUUID);
            } catch (Exception e) {
                channel.basicNack(deliveryTag, false, true);
                throw e;
            }
        } else {
            channel.basicNack(deliveryTag, false, false);
        }
    }
}
