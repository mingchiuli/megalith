package com.chiu.megalith.base.valid;

import com.chiu.megalith.exhibit.service.BlogService;
import com.chiu.megalith.base.lang.Const;
import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import lombok.RequiredArgsConstructor;
import org.springframework.data.redis.core.StringRedisTemplate;

@RequiredArgsConstructor
public class CoopBlogIdConstraintValidator implements ConstraintValidator<CoopBlogId, Long> {

    private final StringRedisTemplate redisTemplate;

    private final BlogService blogService;

    @Override
    public boolean isValid(Long blogId,
                           ConstraintValidatorContext context) {
        return blogService.exist(blogId) &&
                redisTemplate.opsForHash().size(Const.COOP_PREFIX.getInfo() + blogId) < 3;
    }
}
