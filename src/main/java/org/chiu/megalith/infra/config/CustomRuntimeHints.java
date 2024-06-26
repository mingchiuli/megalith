package org.chiu.megalith.infra.config;

import lombok.SneakyThrows;
import org.chiu.megalith.exhibit.cache.mq.CacheBlogEvictMessageListener;
import org.chiu.megalith.exhibit.dto.BlogDescriptionDto;
import org.chiu.megalith.exhibit.dto.BlogExhibitDto;
import org.chiu.megalith.user.cache.mq.CacheEvictMessageListener;
import org.chiu.megalith.user.dto.ButtonDto;
import org.chiu.megalith.user.dto.MenuDto;
import org.chiu.megalith.user.dto.MenusAndButtonsDto;
import org.chiu.megalith.infra.vaild.ListValueConstraintValidator;
import org.chiu.megalith.user.valid.MenuValueConstraintValidator;
import org.chiu.megalith.user.valid.PhoneConstraintValidator;
import org.chiu.megalith.user.valid.UsernameConstraintValidator;
import org.chiu.megalith.security.vo.LoginSuccessVo;
import org.chiu.megalith.security.vo.UserInfoVo;
import org.springframework.aot.hint.*;

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
        hints.reflection().registerMethod(findMethod(CacheBlogEvictMessageListener.class, "handleMessage", Set.class), ExecutableMode.INVOKE);

        hints.reflection().registerConstructor(LinkedHashSet.class.getDeclaredConstructor(), ExecutableMode.INVOKE);
        hints.reflection().registerConstructor(ListValueConstraintValidator.class.getDeclaredConstructor(), ExecutableMode.INVOKE);
        hints.reflection().registerConstructor(PhoneConstraintValidator.class.getDeclaredConstructor(), ExecutableMode.INVOKE);
        hints.reflection().registerConstructor(UsernameConstraintValidator.class.getDeclaredConstructor(), ExecutableMode.INVOKE);
        hints.reflection().registerConstructor(MenuValueConstraintValidator.class.getDeclaredConstructor(), ExecutableMode.INVOKE);

        hints.serialization().registerType(BlogExhibitDto.class);
        hints.serialization().registerType(BlogDescriptionDto.class);
        hints.serialization().registerType(LoginSuccessVo.class);
        hints.serialization().registerType(UserInfoVo.class);
        hints.serialization().registerType(MenusAndButtonsDto.class);
        hints.serialization().registerType(MenuDto.class);
        hints.serialization().registerType(ButtonDto.class);

        hints.reflection().registerType(
                TypeReference.of("com.github.benmanes.caffeine.cache.SSMSA"),
                MemberCategory.PUBLIC_FIELDS, MemberCategory.INVOKE_DECLARED_CONSTRUCTORS, MemberCategory.INVOKE_PUBLIC_METHODS);

        // Register resources
        hints.resources().registerPattern("ValidationMessages.properties");
        hints.resources().registerPattern("script/push-action.lua");
        hints.resources().registerPattern("script/blog-delete.lua");
        hints.resources().registerPattern("script/count-years.lua");
        hints.resources().registerPattern("script/email-phone.lua");
        hints.resources().registerPattern("script/hot-blogs.lua");
        hints.resources().registerPattern("script/list-delete.lua");
        hints.resources().registerPattern("script/password.lua");
        hints.resources().registerPattern("script/push-all.lua");
        hints.resources().registerPattern("script/recover-delete.lua");
        hints.resources().registerPattern("script/statistics.lua");
        hints.resources().registerPattern("script/visit.lua");
    }
}
