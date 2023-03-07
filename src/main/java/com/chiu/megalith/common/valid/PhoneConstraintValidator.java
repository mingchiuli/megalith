package com.chiu.megalith.common.valid;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;


/**
 * @author mingchiuli
 * @create 2023-03-08 1:26 am
 */
public class PhoneConstraintValidator implements ConstraintValidator<Phone, String> {

    @Override
    public boolean isValid(String phone, ConstraintValidatorContext context) {
        return phone.matches("\\d+");
    }
}
