package org.chiu.megalith.search.mq.handler;


import org.chiu.megalith.blog.entity.BlogEntity;
import org.chiu.megalith.blog.repository.BlogRepository;
import org.chiu.megalith.infra.cache.CacheKeyGenerator;
import org.chiu.megalith.infra.lang.Const;
import org.chiu.megalith.infra.search.BlogIndexEnum;
import org.chiu.megalith.infra.search.BlogSearchIndexMessage;

import com.rabbitmq.client.Channel;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;

import org.springframework.amqp.core.Message;
import org.springframework.amqp.rabbit.connection.PublisherCallbackChannel;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.data.redis.core.StringRedisTemplate;
import java.time.LocalDateTime;

@Slf4j
public abstract sealed class BlogIndexSupport permits
        CreateBlogIndexHandler,
        RemoveBlogIndexHandler,
        UpdateBlogIndexHandler {

    protected final StringRedisTemplate redisTemplate;

    protected final BlogRepository blogRepository;

    protected final CacheKeyGenerator cacheKeyGenerator;

    protected final RabbitTemplate rabbitTemplate;

    protected BlogIndexSupport(StringRedisTemplate redisTemplate,
                               BlogRepository blogRepository,
                               CacheKeyGenerator cacheKeyGenerator,
                               RabbitTemplate rabbitTemplate) {
        this.redisTemplate = redisTemplate;
        this.blogRepository = blogRepository;
        this.cacheKeyGenerator = cacheKeyGenerator;
        this.rabbitTemplate = rabbitTemplate;
    }

    public abstract boolean supports(BlogIndexEnum blogIndexEnum);
    protected abstract void redisProcess(BlogEntity blog);
    protected abstract void elasticSearchProcess(BlogEntity blog);

    @SneakyThrows
    public void handle(BlogSearchIndexMessage message, Channel channel, Message msg) {
        String createUUID = msg.getMessageProperties().getHeader(PublisherCallbackChannel.RETURNED_MESSAGE_CORRELATION_KEY);
        long deliveryTag = msg.getMessageProperties().getDeliveryTag();
        if (Boolean.TRUE.equals(redisTemplate.hasKey(Const.CONSUME_MONITOR.getInfo()  + createUUID))) {
            try {
                Long blogId = message.getBlogId();
                Integer year = message.getYear();
                BlogEntity blogEntity = blogRepository.findById(blogId)
                        .orElseGet(() -> BlogEntity.builder()
                                .id(blogId)
                                .created(LocalDateTime.of(year, 1,1,1,1,1, 1))
                                .build());

                redisProcess(blogEntity);
                elasticSearchProcess(blogEntity);
                //手动签收消息
                //false代表不是批量签收模式
                channel.basicAck(deliveryTag, false);
                redisTemplate.delete(Const.CONSUME_MONITOR.getInfo() + createUUID);
            } catch (Exception e) {
                channel.basicNack(deliveryTag, false, true);
                log.error("consume failure", e);
            }
        } else {
            channel.basicNack(deliveryTag, false, false);
        }
    }
}
