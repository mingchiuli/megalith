package org.chiu.megalith.infra.config;

import lombok.SneakyThrows;
import org.chiu.megalith.infra.listener.CacheMessageListener;
import org.chiu.megalith.infra.valid.ListValueConstraintValidator;
import org.chiu.megalith.infra.valid.PhoneConstraintValidator;
import org.chiu.megalith.infra.valid.UsernameConstraintValidator;
import org.chiu.megalith.security.role.DefaultRoleHolder;
import org.chiu.megalith.security.role.HighestRoleHolder;
import org.chiu.megalith.security.vo.LoginSuccessVo;
import org.chiu.megalith.security.vo.UserInfoVo;
import org.springframework.aot.hint.ExecutableMode;
import org.springframework.aot.hint.RuntimeHints;
import org.springframework.aot.hint.RuntimeHintsRegistrar;
import org.springframework.util.ReflectionUtils;

import java.util.LinkedHashSet;
import java.util.Set;

public class CustomRuntimeHints implements RuntimeHintsRegistrar {
    @SneakyThrows
    @Override// Register method for reflection
    public void registerHints(RuntimeHints hints, ClassLoader classLoader) {
        // Register method for reflection
        hints.reflection().registerMethod(ReflectionUtils.findMethod(CacheMessageListener.class, "handleMessage", Set.class), ExecutableMode.INVOKE);
        hints.reflection().registerMethod(ReflectionUtils.findMethod(HighestRoleHolder.class, "getRole"), ExecutableMode.INVOKE);
        hints.reflection().registerMethod(ReflectionUtils.findMethod(DefaultRoleHolder.class, "getRole"), ExecutableMode.INVOKE);

        hints.reflection().registerConstructor(LinkedHashSet.class.getDeclaredConstructor(), ExecutableMode.INVOKE);
        hints.reflection().registerConstructor(ListValueConstraintValidator.class.getDeclaredConstructor(), ExecutableMode.INVOKE);
        hints.reflection().registerConstructor(PhoneConstraintValidator.class.getDeclaredConstructor(), ExecutableMode.INVOKE);
        hints.reflection().registerConstructor(UsernameConstraintValidator.class.getDeclaredConstructor(), ExecutableMode.INVOKE);

        hints.serialization().registerType(LoginSuccessVo.class);
        hints.serialization().registerType(UserInfoVo.class);

        // Register resources
        hints.resources().registerPattern("ValidationMessages.properties");
    }
}
