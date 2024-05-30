package org.chiu.megalith.security.user;

import org.chiu.megalith.user.entity.UserEntity;
import org.chiu.megalith.user.repository.UserRepository;
import lombok.RequiredArgsConstructor;
import org.chiu.megalith.user.service.UserRoleService;
import org.springframework.security.core.authority.AuthorityUtils;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Component;

import java.util.List;

import static org.chiu.megalith.infra.lang.Const.ROLE_PREFIX;
import static org.chiu.megalith.infra.lang.ExceptionMessage.*;


@Component
@RequiredArgsConstructor
public final class UserDetailsServiceImpl implements UserDetailsService {

	private final UserRepository userRepository;

	private final UserRoleService userRoleService;

	@Override
	public UserDetails loadUserByUsername(String username) throws UsernameNotFoundException {
		return loadUserByUsernameFromDb(username);
	}

	private LoginUser loadUserByUsernameFromDb(String username) {

		UserEntity user = userRepository.findByUsernameOrEmailOrPhone(username, username, username)
				.orElseThrow(() -> new UsernameNotFoundException(USER_NOT_EXIST.getMsg()));

		Long userId = user.getId();
		List<String> roleCodes = userRoleService.findRoleCodesDecorByUserId(userId);

		//通过User去自动比较用户名和密码
		return new LoginUser(username,
				user.getPassword(),
				true,
				true,
				true,
				user.getStatus() == 0,
				AuthorityUtils.createAuthorityList(roleCodes),
				userId);
	}
}
