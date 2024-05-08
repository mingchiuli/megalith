package org.chiu.megalith.infra.config;

import lombok.SneakyThrows;
import org.chiu.megalith.blog.cache.mq.CacheBlogEvictMessageListener;
import org.chiu.megalith.manage.cache.mq.CacheEvictMessageListener;
import org.chiu.megalith.manage.valid.ListValueConstraintValidator;
import org.chiu.megalith.manage.valid.MenuValueConstraintValidator;
import org.chiu.megalith.manage.valid.PhoneConstraintValidator;
import org.chiu.megalith.manage.valid.UsernameConstraintValidator;
import org.chiu.megalith.security.vo.LoginSuccessVo;
import org.chiu.megalith.security.vo.UserInfoVo;
import org.springframework.aot.hint.ExecutableMode;
import org.springframework.aot.hint.RuntimeHints;
import org.springframework.aot.hint.RuntimeHintsRegistrar;

import java.util.LinkedHashSet;
import java.util.Set;

import static org.springframework.util.ReflectionUtils.*;

@SuppressWarnings("all")
public class CustomRuntimeHints implements RuntimeHintsRegistrar {
    @SneakyThrows
    @Override// Register method for reflection
    public void registerHints(RuntimeHints hints, ClassLoader classLoader) {
        // Register method for reflection
        hints.reflection().registerMethod(findMethod(CacheEvictMessageListener.class, "handleMessage", Set.class), ExecutableMode.INVOKE);

        hints.reflection().registerConstructor(LinkedHashSet.class.getDeclaredConstructor(), ExecutableMode.INVOKE);
        hints.reflection().registerConstructor(ListValueConstraintValidator.class.getDeclaredConstructor(), ExecutableMode.INVOKE);
        hints.reflection().registerConstructor(PhoneConstraintValidator.class.getDeclaredConstructor(), ExecutableMode.INVOKE);
        hints.reflection().registerConstructor(UsernameConstraintValidator.class.getDeclaredConstructor(), ExecutableMode.INVOKE);
        hints.reflection().registerConstructor(MenuValueConstraintValidator.class.getDeclaredConstructor(), ExecutableMode.INVOKE);


        hints.serialization().registerType(LoginSuccessVo.class);
        hints.serialization().registerType(UserInfoVo.class);

        // Register resources
        hints.resources().registerPattern("ValidationMessages.properties");
        hints.resources().registerPattern("script/push-action.lua");

    }
}
