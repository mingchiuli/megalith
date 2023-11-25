package org.chiu.megalith.security.user;

import org.chiu.megalith.manage.entity.UserEntity;
import org.chiu.megalith.manage.repository.UserRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.authority.AuthorityUtils;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Component;

import static org.chiu.megalith.infra.lang.Const.ROLE_PREFIX;
import static org.chiu.megalith.infra.lang.ExceptionMessage.*;


@Component
@RequiredArgsConstructor
public final class UserDetailsServiceImpl implements UserDetailsService {

	private final UserRepository userRepository;

	@Override
	public UserDetails loadUserByUsername(String username) throws UsernameNotFoundException {
		return loadUserByUsernameFromDb(username);
	}

	private LoginUser loadUserByUsernameFromDb(String username) {

		UserEntity user;

		if (username.contains("@")) {
            user = userRepository.findByEmail(username)
                    .orElseThrow(() -> new UsernameNotFoundException(EMAIL_NOT_EXIST.getMsg()));
		} else if (username.matches("\\d+")){
            user = userRepository.findByPhone(username)
                    .orElseThrow(() -> new UsernameNotFoundException(PHONE_NOT_EXIST.getMsg()));
		} else {
            user = userRepository.findByUsername(username)
                    .orElseThrow(() -> new UsernameNotFoundException(USER_NOT_EXIST.getMsg()));
		}

		//通过User去自动比较用户名和密码
		return new LoginUser(username,
				user.getPassword(),
				true,
				true,
				true,
				user.getStatus() == 0,
				AuthorityUtils.createAuthorityList(ROLE_PREFIX.getInfo() + user.getRole()),
				user.getId());
	}
}
