package org.chiu.megalith.manage.wrapper;

import lombok.RequiredArgsConstructor;
import org.chiu.megalith.manage.cache.CacheEvict;
import org.chiu.megalith.manage.cache.handler.AllAuthorityCacheEvictHandler;
import org.chiu.megalith.manage.entity.AuthorityEntity;
import org.chiu.megalith.manage.repository.AuthorityRepository;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@RequiredArgsConstructor
public class AuthorityWrapper {

    private final AuthorityRepository authorityRepository;

    @CacheEvict(handler = { AllAuthorityCacheEvictHandler.class })
    public void save(AuthorityEntity authorityEntity) {
        authorityRepository.save(authorityEntity);
    }

    @CacheEvict(handler = { AllAuthorityCacheEvictHandler.class })
    public void deleteAllById(List<Long> ids) {
        authorityRepository.deleteAllById(ids);
    }
}
