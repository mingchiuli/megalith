package com.chiu.megalith.common.valid;

import com.chiu.megalith.blog.service.BlogService;
import com.chiu.megalith.common.lang.Const;
import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import lombok.RequiredArgsConstructor;
import org.springframework.data.redis.core.StringRedisTemplate;

@RequiredArgsConstructor
public class CoopBlogIdConstraintValidator implements ConstraintValidator<CoopBlogId, Long> {

    private final StringRedisTemplate redisTemplate;

    private final BlogService blogService;


    @Override
    public void initialize(CoopBlogId constraintAnnotation) {
        ConstraintValidator.super.initialize(constraintAnnotation);
    }

    @Override
    public boolean isValid(Long blogId, ConstraintValidatorContext constraintValidatorContext) {
        return blogService.exist(blogId) && redisTemplate.opsForHash().size(Const.COOP_PREFIX.getMsg() + blogId) < 3;
    }
}
