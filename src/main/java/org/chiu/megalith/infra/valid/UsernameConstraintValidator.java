package org.chiu.megalith.infra.valid;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import org.springframework.util.StringUtils;


/**
 * @author mingchiuli
 * @create 2023-03-08 1:11 am
 */
public class UsernameConstraintValidator implements ConstraintValidator<Username, String> {

    @Override
    public boolean isValid(String username,
                           ConstraintValidatorContext context) {
        if (StringUtils.hasLength(username)) {
            return !username.matches("\\d+") && !username.contains("@");
        }
        return false;
    }
}

