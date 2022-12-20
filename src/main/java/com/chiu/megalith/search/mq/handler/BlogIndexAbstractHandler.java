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
import java.util.Optional;


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
        if (Boolean.TRUE.equals(redisTemplate.hasKey(Const.CONSUME_MONITOR.getMsg()  + createUUID))) {
            try {
                Long blogId = message.getBlogId();
                Integer year = message.getYear();
                Optional<BlogEntity> blog = blogRepository.findById(blogId);
                BlogEntity blogEntity = blog.orElseGet(() ->
                        BlogEntity.
                                builder().
                                id(blogId).
                                created(LocalDateTime.of(year, 1,1,1 ,1 ,1, 1)).
                                build());

                redisProcess(blogEntity);
                elasticSearchProcess(blogEntity);
                long deliveryTagCreate = msg.getMessageProperties().getDeliveryTag();
                //手动签收消息
                //false代表不是批量签收模式
                channel.basicAck(deliveryTagCreate, false);
            } finally {
                redisTemplate.delete(Const.CONSUME_MONITOR.getMsg() + createUUID);
            }
        } else {
            long deliveryTag = msg.getMessageProperties().getDeliveryTag();
            channel.basicNack(deliveryTag, false, false);
        }
    }
}
