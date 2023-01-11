package com.chiu.megalith.search.mq.handler;


import com.chiu.megalith.blog.entity.BlogEntity;
import com.chiu.megalith.blog.repository.BlogRepository;
import com.chiu.megalith.common.lang.Const;
import com.chiu.megalith.common.search.BlogIndexEnum;
import com.chiu.megalith.common.search.BlogSearchIndexMessage;
import com.rabbitmq.client.Channel;
import lombok.SneakyThrows;
import org.springframework.amqp.core.Message;
import org.springframework.amqp.rabbit.connection.PublisherCallbackChannel;
import org.springframework.data.redis.core.StringRedisTemplate;
import java.time.LocalDateTime;


public abstract class BlogIndexAbstractHandler {

    protected BlogIndexAbstractHandler(StringRedisTemplate redisTemplate, BlogRepository blogRepository) {
        this.redisTemplate = redisTemplate;
        this.blogRepository = blogRepository;
    }

    protected StringRedisTemplate redisTemplate;
    protected BlogRepository blogRepository;

    public abstract boolean supports(BlogIndexEnum blogIndexEnum);
    protected abstract void redisProcess(BlogEntity blog);
    protected abstract void elasticSearchProcess(BlogEntity blog);

    @SneakyThrows
    public void handle(BlogSearchIndexMessage message, Channel channel, Message msg) {
        String createUUID = msg.getMessageProperties().getHeader(PublisherCallbackChannel.RETURNED_MESSAGE_CORRELATION_KEY);
        long deliveryTag = msg.getMessageProperties().getDeliveryTag();
        if (Boolean.TRUE.equals(redisTemplate.hasKey(Const.CONSUME_MONITOR.getMsg()  + createUUID))) {
            try {
                Long blogId = message.getBlogId();
                Integer year = message.getYear();
                BlogEntity blogEntity = blogRepository.findById(blogId).
                        orElseGet(() -> BlogEntity.builder().
                                id(blogId).
                                created(LocalDateTime.of(year, 1,1,1 ,1 ,1, 1)).
                                build()
                        );

                redisProcess(blogEntity);
                elasticSearchProcess(blogEntity);
                //手动签收消息
                //false代表不是批量签收模式
                channel.basicAck(deliveryTag, false);
                redisTemplate.delete(Const.CONSUME_MONITOR.getMsg() + createUUID);
            } catch (Exception e) {
                channel.basicNack(deliveryTag, false, true);
                throw e;
            }
        } else {
            channel.basicNack(deliveryTag, false, false);
        }
    }
}
