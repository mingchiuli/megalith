package com.chiu.megalith.exhibit.cache;

import com.chiu.megalith.base.lang.Const;

import java.lang.annotation.*;

/**
 * @author mingchiuli
 * @create 2021-12-01 7:45 AM
 */
@Target({ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface Cache {

    int expire() default 120;

    Const prefix();

}
