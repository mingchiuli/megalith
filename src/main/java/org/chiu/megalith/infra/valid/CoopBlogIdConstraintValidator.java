package org.chiu.megalith.infra.valid;

import org.chiu.megalith.blog.service.BlogService;
import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
public class CoopBlogIdConstraintValidator implements ConstraintValidator<CoopBlogId, Long> {

    private final BlogService blogService;

    @Override
    public boolean isValid(Long blogId,
                           ConstraintValidatorContext context) {
        return blogService.exist(blogId);
    }
}
