package org.chiu.megalith.user.cache.handler;

import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import org.chiu.megalith.user.cache.CacheEvictHandler;
import org.chiu.megalith.infra.cache.CacheKeyGenerator;
import org.chiu.megalith.user.repository.RoleRepository;
import org.chiu.megalith.user.wrapper.RoleMenuWrapper;
import org.springframework.stereotype.Component;

import java.lang.reflect.Method;
import java.util.HashSet;
import java.util.List;
import java.util.Set;


@RequiredArgsConstructor
@Component
public class AllMenuAndButtonCacheEvictHandler extends CacheEvictHandler {

    private final RoleRepository roleRepository;

    private final CacheKeyGenerator cacheKeyGenerator;

    @SneakyThrows
    @Override
    public Set<String> handle(Object[] args) {
        List<String> roleList = roleRepository.findAllCodes();

        Method method = RoleMenuWrapper.class.getMethod("getCurrentRoleNav", List.class);
        Set<String> set = new HashSet<>();
        roleList.forEach(role -> set.add(cacheKeyGenerator.generateKey(method, role)));
        return set;
    }
}
