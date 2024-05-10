package org.chiu.megalith.authority.cache.handler;

import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import org.chiu.megalith.authority.cache.CacheEvictHandler;
import org.chiu.megalith.infra.cache.CacheKeyGenerator;
import org.chiu.megalith.authority.repository.RoleRepository;
import org.chiu.megalith.authority.wrapper.RoleMenuWrapper;
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

        Method method = RoleMenuWrapper.class.getMethod("getCurrentRoleNav", String.class);
        Set<String> set = new HashSet<>();
        roleList.forEach(role -> set.add(cacheKeyGenerator.generateKey(method, role)));
        return set;
    }
}
