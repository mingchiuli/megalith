package org.chiu.megalith.search.mq.handler;


import org.chiu.megalith.manage.entity.BlogEntity;
import org.chiu.megalith.blog.repository.BlogRepository;
import org.chiu.megalith.infra.lang.Const;
import org.chiu.megalith.infra.search.BlogIndexEnum;
import org.chiu.megalith.infra.search.BlogSearchIndexMessage;

import com.rabbitmq.client.Channel;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;

import org.springframework.amqp.core.Message;
import org.springframework.amqp.rabbit.connection.PublisherCallbackChannel;
import org.springframework.data.redis.core.StringRedisTemplate;

@Slf4j
public abstract sealed class BlogIndexSupport permits
        CreateBlogIndexHandler,
        RemoveBlogIndexHandler,
        UpdateBlogIndexHandler {

    protected final StringRedisTemplate redisTemplate;

    protected final BlogRepository blogRepository;

    protected BlogIndexSupport(StringRedisTemplate redisTemplate,
                               BlogRepository blogRepository) {
        this.redisTemplate = redisTemplate;
        this.blogRepository = blogRepository;
    }

    public abstract boolean supports(BlogIndexEnum blogIndexEnum);
    protected abstract void elasticSearchProcess(BlogEntity blog);

    @SneakyThrows
    public void handle(BlogSearchIndexMessage message, Channel channel, Message msg) {
        String createUUID = msg.getMessageProperties().getHeader(PublisherCallbackChannel.RETURNED_MESSAGE_CORRELATION_KEY);
        long deliveryTag = msg.getMessageProperties().getDeliveryTag();
        if (Boolean.TRUE.equals(redisTemplate.hasKey(Const.CONSUME_MONITOR.getInfo()  + createUUID))) {
            try {
                Long blogId = message.getBlogId();
                BlogEntity blogEntity = blogRepository.findById(blogId)
                        .orElseGet(() -> BlogEntity.builder()
                                .id(blogId)
                                .build());

                elasticSearchProcess(blogEntity);
                //手动签收消息
                //false代表不是批量签收模式
                channel.basicAck(deliveryTag, false);
                redisTemplate.delete(Const.CONSUME_MONITOR.getInfo() + createUUID);
            } catch (Exception e) {
                log.error("consume failure", e);
                channel.basicNack(deliveryTag, false, true);
            }
        } else {
            channel.basicNack(deliveryTag, false, false);
        }
    }
}
