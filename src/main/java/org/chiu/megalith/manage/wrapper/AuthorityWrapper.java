package org.chiu.megalith.manage.wrapper;

import lombok.RequiredArgsConstructor;
import org.chiu.megalith.infra.cache.CacheEvict;
import org.chiu.megalith.infra.lang.Const;
import org.chiu.megalith.manage.entity.AuthorityEntity;
import org.chiu.megalith.manage.repository.AuthorityRepository;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@RequiredArgsConstructor
public class AuthorityWrapper {

    private final AuthorityRepository authorityRepository;

    @CacheEvict(prefix = {Const.HOT_AUTHORITIES})
    public void save(AuthorityEntity authorityEntity) {
        authorityRepository.save(authorityEntity);
    }

    @CacheEvict(prefix = {Const.HOT_AUTHORITIES})
    public void deleteAllById(List<Long> ids) {
        authorityRepository.deleteAllById(ids);
    }
}
